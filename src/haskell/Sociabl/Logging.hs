module Sociabl.Logging          
       ( defaultLogger
       , defaultLoggerFormat
       , initLogger ) where
       
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler (LogHandler, setFormatter)
import System.Log.Handler.Simple       

defaultLogger :: String
defaultLogger = "Sociabl.Default"

defaultLoggerFormat :: String
defaultLoggerFormat = "[$time : $loggername : $prio] $msg"

initLogger :: IO ()
initLogger = do
  updateGlobalLogger rootLoggerName (setLevel EMERGENCY)
  updateGlobalLogger defaultLogger (setLevel INFO)
  h <- fileHandler "log/sociabl.log" INFO >>= 
       (\h -> return $ setFormatter h (simpleLogFormatter defaultLoggerFormat))
  updateGlobalLogger defaultLogger (addHandler h)


