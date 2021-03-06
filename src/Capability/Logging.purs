module SmsnWeb.Capability.Logging where

import Prelude

import Control.Logger (Logger)
import Control.Logger.Console as Logger
import Control.Monad.Trans.Class (lift)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (toUpper) as String
import Effect.Class (class MonadEffect)
import Halogen (HalogenM)
import SmsnWeb.Capability.Now (class Now, nowDateTime)

data Severity
  = Debug
  | Info
  | Warning
  | Error

derive instance eqSeverity ∷ Eq Severity
derive instance ordSeverity ∷ Ord Severity
derive instance genericSeverity ∷ Generic Severity _

instance showSeverity ∷ Show Severity where
  show = genericShow

type Message =
  { severity ∷ Severity
  , timestamp ∷ DateTime
  , text ∷ String
  }

class Now m <= Logging m where
  logMessage ∷ Message -> m Unit

instance loggingHalogenM ∷ Logging m => Logging (HalogenM state action slots output m) where
  logMessage = lift <<< logMessage

mkMessage ∷ forall m. Now m => Severity -> String -> m Message
mkMessage severity msg = do
  timestamp ← nowDateTime
  let
    header = "[" <> fmtSeverity severity <> ": " <> fmtTimestamp timestamp <> "]"
    text = header <> "\n" <> msg
  pure { timestamp, severity, text }
  where
    fmtSeverity ∷ Severity -> String
    fmtSeverity = String.toUpper <<< show

    fmtTimestamp ∷ DateTime -> String
    fmtTimestamp =
      either (const "(Failed to assign time)") identity <<<
      formatDateTime "YYYY-DD-MM hh:mm:ss a"

consoleLogger ∷ forall m. MonadEffect m => Logger m Message
consoleLogger = Logger.console _.text

log ∷ forall m. Logging m => Severity -> String -> m Unit
log s = logMessage <=< mkMessage s

logDebug ∷ forall m. Logging m => String -> m Unit
logDebug = log Debug

logInfo ∷ forall m. Logging m => String -> m Unit
logInfo = log Info

logWarning ∷ forall m. Logging m => String -> m Unit
logWarning = log Warning

logError ∷ forall m. Logging m => String -> m Unit
logError = log Error
