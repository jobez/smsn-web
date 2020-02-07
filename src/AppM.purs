module SmsnWeb.AppM where

import Prelude

import Control.Logger (Logger)
import Control.Logger as Logger
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fromArray, jsonEmptyObject, stringify, (.!=), (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Array (elem)
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
import SmsnWeb.Capability.WebsocketM (class WebsocketM)
import SmsnWeb.Capability.Logging (Severity(..)) as Severity
import SmsnWeb.Capability.Logging (class Logging, Message, Severity, consoleLogger, logDebug)

import SmsnWeb.Capability.Now (class Now)
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


instance nowAppM ∷ Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance loggingAppM ∷ Logging AppM where
  logMessage msg = do
    level <- asks _.logLevel
    Logger.log (filter level consoleLogger) msg
    where
      filter ∷ LogLevel -> Logger AppM Message -> Logger AppM Message
      filter Dev = identity
      filter Prod = Logger.cfilter isImportant

      isImportant ∷ Message -> Boolean
      isImportant e = e.severity `elem` important

      important ∷ Array Severity
      important = [Severity.Warning, Severity.Error]


instance websocketAppM :: WebsocketM AppM where
  connect url = do
      env <- ask
      logDebug $ "Setup ws to: " <> env.baseUrl
      liftEffect $ do


        connection <- WS.create env.baseUrl []
    --socket.onopen $= \event -> do
        pure unit
  disconnect =
    liftEffect $ do pure unit
