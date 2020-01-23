module SmsnWeb.Capability.WebsocketM where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)


class Monad m <= WebsocketM m where
  connect    :: String -> m Unit
  disconnect :: m Unit

instance websocketHalogenM :: WebsocketM m => WebsocketM (HalogenM st act slots msg m) where
  connect    = lift <<< connect
  disconnect = lift disconnect
