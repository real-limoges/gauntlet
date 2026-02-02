module Log (logError, logWarning, logInfo) where

import System.IO (hPutStrLn, stderr)

logError :: String -> IO ()
logError msg = hPutStrLn stderr $ "[!] [ERROR] " ++ msg

logWarning :: String -> IO ()
logWarning msg = hPutStrLn stderr $ "[!] [WARNING] " ++ msg

logInfo :: String -> IO ()
logInfo msg = hPutStrLn stderr $ "[INFO] " ++ msg
