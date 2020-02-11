module SmsnWeb.Component.Root
  ( IO
  , State
  , Query(..)
  , Action(..)
  -- , Input
  , SmsnAction
  , wsSender
  , wsProducer
  , wsConsumer
  , component
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Reader.Trans (ask)
import Data.Argonaut (class EncodeJson, encodeJson, jsonEmptyObject, stringify, (:=), (~>))
import Data.Array as A
import Data.Const (Const)
import Data.Either (either, hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Foreign (F, Foreign, readString, unsafeToForeign)
import Halogen (HalogenIO, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routing.Duplex (parse)
import Routing.Hash (getHash)
import SmsnWeb.Capability.Logging (class Logging, logDebug, logMessage)
import SmsnWeb.Capability.WebsocketM (class WebsocketM, connect)
import SmsnWeb.Env (Env)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS


type IO = HalogenIO Query SmsnAction Aff

type Slot = H.Slot Query SmsnAction

data Query a = ReceiveMessage String a

data SmsnAction = FindRoot

type State =
  { messages :: Array String
  , inputText :: String
  }

-- type State =
--   { route :: Routing
--   }

-- data Query a = Navigate Route a
type Filter =  {minSource :: String
               , defaultSource :: String
               , minWeight :: Number
               , defaultWeight :: Number
               , titleCutoff :: Int
               , style :: String}


type Act = { action :: String
           , height :: Int
           , filter :: Filter

           }


type Args =  {
  gremlin :: Act
  , language :: String
  , session :: String}

type WSMsg =
  {
   op :: String
  , processor :: String
  , args :: Args
  }


-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: WS.WebSocket -> CR.Producer String Aff Unit
wsProducer socket = CRA.produce \emitter -> do
  listener <- EET.eventListener \ev -> do
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
        emit emitter msg
  EET.addEventListener
    WSET.onMessage
    listener
    false
    (WS.toEventTarget socket)
  where
    readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
    readHelper read =
      either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ReceiveMessage` queries in when it receives inputs from the
-- producer.
wsConsumer :: (forall a. Query a -> Aff (Maybe a)) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
  void $ query $ H.tell $ ReceiveMessage msg
  pure Nothing

-- A consumer coroutine that takes output messages from our component IO
-- and sends them using the websocket

wsSender :: WS.WebSocket -> CR.Consumer SmsnAction Aff Unit
wsSender socket = CR.consumer \msg -> do
  case msg of
    FindRoot ->
      liftEffect $ WS.sendString socket $ stringify gremlinJson
      where gremlinMsg =  {op: "eval",
                           processor: "session",
                           args: argz}
            argz = {gremlin: findRoot, language: "smsn", session: "undefined"}
            findRoot = { action: "net.fortytwo.smsn.server.actions.NoAction"
                       , height: 2
                       , filter: filter}
            filter =  {minSource: "private"
                      , defaultSource: "private"
                      , minWeight: 0.0
                      , defaultWeight: 0.5
                      , titleCutoff: 100
                      , style: "forward"}
            gremlinJson = encodeJson gremlinMsg
  pure Nothing


data Action = Initialize
            | HandleInput String
            | Connect String
            | Submit Event
-- type Input = Maybe Route

-- data Output = Void

type OpaqueSlot = H.Slot (Const Unit) Void

type ChildSlots =
  ( home       :: OpaqueSlot Unit
  , navigation :: OpaqueSlot Unit
  -- , login      :: H.Slot (Const Unit) Login.LoginMsg Unit
  )

_home = SProxy :: SProxy "home"
_navigation = SProxy :: SProxy "navigation"
_login = SProxy :: SProxy "login"

type HTML m = H.ComponentHTML Action ChildSlots m
type ComponentM m eff = H.HalogenM State Action ChildSlots SmsnAction m eff
type QueryHandler m = forall a. Query a -> ComponentM m (Maybe a)
type ActionHandler m = Action -> ComponentM m Unit

component
  :: forall i m
   . MonadAff m
  => MonadAsk Env m
  => WebsocketM m
  => MonadEffect m
  => Logging m
  => H.Component HH.HTML Query i SmsnAction m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery  = handleQuery
    , initialize   = Just Initialize
    }
  }
  where
  initialState :: forall i. i -> State
  initialState _ =
    { messages: [],
      inputText: ""
    }

  handleAction :: ActionHandler m
  handleAction = case _ of
    Initialize -> do
      pure unit

    Connect s -> do
      env <- ask
      logDebug $ "clicked button " <> env.baseUrl
    HandleInput text -> do
      H.modify_ (_ { inputText = text })
    Submit ev -> do
      H.liftEffect $ Event.preventDefault ev
      st <- H.get
      let outgoingMessage = st.inputText
      H.raise $ FindRoot
      H.modify_ \st' -> st'
        { messages = st'.messages `A.snoc` ("Sending: " <> outgoingMessage)
      , inputText = ""
      }
      --   pure unit


  handleQuery :: QueryHandler m
  handleQuery = case _ of
    ReceiveMessage msg a -> do
      let incomingMessage = "Received: " <> msg
      H.modify_ \st -> st { messages = st.messages `A.snoc` incomingMessage }
      pure (Just a)



  render :: State -> HTML m
  render state =
    HH.form
    [ HE.onSubmit (Just <<< Submit) ]
    [ HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.messages
    , HH.input
        [ HP.type_ HP.InputText
        , HP.value (state.inputText)
        , HE.onValueInput (Just <<< HandleInput)
        ]
    , HH.button
        [ HP.type_ HP.ButtonSubmit ]
        [ HH.text "Send Message" ]
    ]
    -- HH.button
    --   [ HP.title "hey"
    --   , HE.onClick \_ -> Just $ Connect "foo"
    --   ]
    --   [ HH.text "Socket" ]




-- handleLoginMsg :: Login.LoginMsg -> Maybe Action
-- handleLoginMsg (Login.LoggedIn username) =
--   Just $ Connect $ "ws://echo.websocket.net" <> "/" <> username
