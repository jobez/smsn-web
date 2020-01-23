module SmsnWeb.Data.SmSn
       ( TableName, ColumnName
                    -- , Statement(..)
                    -- , Transaction(..)
       , Timestamp
       , AtomId
       , ZeroToOne
       , Atom(..)
       , AtomWithSource(..)
       ) where

import Data.List as L
import Data.Maybe (Maybe)

type Timestamp = Int

type TableName = String
type ColumnName = String

-- data Statement = Statement
--   { stmtTable :: TableName
--   , stmtData :: M.Map ColumnName Value
--   } deriving Show


-- data Transaction
--   = Add    Timestamp Statement
--   | Remove Timestamp Statement deriving Show

type AtomId = String
type ZeroToOne = Number

data Atom = Atom
  { atomId       :: AtomId
  , atomCreated  :: Timestamp
  , atomChildren :: L.List AtomId
  , atomText     :: Maybe String
  , atomTitle    :: Maybe String
  , atomAlias    :: Maybe String
  , atomShortcut :: Maybe String
  , atomWeight   :: ZeroToOne
  , atomPriority :: ZeroToOne
  }

data AtomWithSource = AtomWithSource Atom String
