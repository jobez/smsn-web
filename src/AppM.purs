module SmsnWeb.AppM where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fromArray, jsonEmptyObject, stringify, (.!=), (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Encode (encodeJson)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Effect.Ref as Ref
import Example.Driver.Websockets.Log as Log
import Foreign (F, Foreign, unsafeToForeign, readString)
import Halogen as H
import SmsnWeb.Capability.Graph (class GraphM)
import SmsnWeb.Env (LogLevel(..), Env)
import Type.Equality (class TypeEquals, from)
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS



newtype AppM a = AppM (ReaderT Env Aff a)


runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM


instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

newtype Args = Args {
  gremlin :: String
  , language :: String
    }


newtype WSMsg = WSMsg
  { id :: String
  , op :: String
  , processor :: String
  , args :: Args
  }

instance encodeArgs :: EncodeJson Args where
  encodeJson (Args args)
     = "gremlin" := args.gremlin
    ~> "language" := args.language
    ~> "bindings" := jsonEmptyObject

instance encodeWSMsg :: EncodeJson WSMsg where
  encodeJson (WSMsg msg)
     = "requestId" := msg.id
    ~> "op" := msg.op
    ~> "processor" := msg.processor
    ~> "args" := msg.args
    ~> jsonEmptyObject

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
wsConsumer :: (forall a. Log.Query a -> Aff (Maybe a)) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
  void $ query $ H.tell $ Log.ReceiveMessage msg
  pure Nothing

-- A consumer coroutine that takes output messages from our component IO
-- and sends them using the websocket
wsSender :: WS.WebSocket -> CR.Consumer Log.Message Aff Unit
wsSender socket = CR.consumer \msg -> do
  case msg of
    Log.OutputMessage msgContents ->
      liftEffect $ WS.sendString socket $ stringify gremlinJson
      where gremlinMsg = WSMsg {id: "",
                                op: "eval",
                                processor: "",
                                args: argz}
            argz = Args {gremlin: msgContents, language: "gremlin-groovy"}
            gremlinJson = encodeJson gremlinMsg
  pure Nothing
