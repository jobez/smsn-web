module SmsnWeb.Capability.Graph where

import Prelude
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

data Graph = Undefined

class Monad m <= GraphM m where
  readGraph :: m Graph

instance graphHalogenM :: GraphM m => GraphM (HalogenM st act slots msg m) where
  readGraph = lift readGraph
