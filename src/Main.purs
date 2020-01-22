module Main where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.UUID (genUUID, genv3UUID, genv5UUID, parseUUID, toString)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Example.Driver.Websockets.Log as Log
import Foreign (F, Foreign, unsafeToForeign, readString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import SmsnWeb.Env (LogLevel(..), Env)

main :: Effect Unit
main = do


  -- connection <- WS.create "ws://localhost:8182/gremlin" []
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      baseUrl = "ws://localhost:8182/gremlin"
      logLevel = Dev
      environment :: Env
      environment = { baseUrl, logLevel }
      -- rootComponent :: H.Component HH.HTML Router.Query {} Void Aff
      -- rootComponent = H.hoist (runAppM environment) Router.component

    runUI Log.component unit body

    -- The wsSender consumer subscribes to all output messages
    -- from our component
    -- io.subscribe $ wsSender connection

    -- -- Connecting the consumer to the producer initializes both,
    -- -- feeding queries back to our component as messages are received.
    -- CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query)
