module Main where

import Control.Exception (SomeException, fromException, try)
import Foreign.C (CInt (..))
import Lib (run)
import System.Exit (ExitCode (..))
import System.IO (hFlush, hPutStrLn, stderr, stdout)

-- | Bypass GHC's normal shutdown which waits for finalizers and lingering
-- threads (e.g. http-client Manager reaper, HTTP/2 connection teardown).
foreign import ccall "exit" c_exit :: CInt -> IO ()

main :: IO ()
main = do
    result <- try run :: IO (Either SomeException ())
    hFlush stdout
    hFlush stderr
    case result of
        Right () -> c_exit 0
        Left e -> case fromException e of
            Just ExitSuccess -> c_exit 0
            Just (ExitFailure n) -> c_exit (fromIntegral n)
            Nothing -> do
                hPutStrLn stderr $ "Error: " ++ show e
                hFlush stderr
                c_exit 1
