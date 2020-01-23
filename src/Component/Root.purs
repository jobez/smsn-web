module SmsnWeb.Component.Root
  ( IO
  , State
  , Query(..)
  , Action(..)
  -- , Input
  , Output
  , component
  ) where

import Prelude

import Control.Monad.Reader (class MonadAsk)
import Data.Array as A
import Data.Either (hush)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenIO, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Routing.Duplex (parse)
import Routing.Hash (getHash)
import SmsnWeb.Capability.WebsocketM (class WebsocketM, connect)
import SmsnWeb.Env (Env)


type IO = HalogenIO Query Output Aff

type State =
  { messages :: Array String
  , inputText :: String
  }

-- type State =
--   { route :: Routing
--   }

-- data Query a = Navigate Route a
data Query a = ReceiveMessage String a

data Action = Initialize | Connect String
-- type Input = Maybe Route

data Output = Void

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
type ComponentM m eff = H.HalogenM State Action ChildSlots Output m eff
type QueryHandler m = forall a. Query a -> ComponentM m (Maybe a)
type ActionHandler m = Action -> ComponentM m Unit

component
  :: forall i m
   . MonadAff m
  => MonadAsk Env m
  -- => Navigate m
  => WebsocketM m
  => H.Component HH.HTML Query i Output m
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

    Connect s -> connect s

  handleQuery :: QueryHandler m
  handleQuery = case _ of
    ReceiveMessage msg a -> do
      let incomingMessage = "Received: " <> msg
      H.modify_ \st -> st { messages = st.messages `A.snoc` incomingMessage }
      pure (Just a)



  render :: State -> HTML m
  render state =
    HH.button
      [ HP.title "hey"
      , HE.onClick \_ -> Just $ Connect "foo"
      ]
      [ HH.text "Socket" ]




-- handleLoginMsg :: Login.LoginMsg -> Maybe Action
-- handleLoginMsg (Login.LoggedIn username) =
--   Just $ Connect $ "ws://echo.websocket.net" <> "/" <> username
