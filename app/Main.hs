module Main where

import Control.Exception (Handler (..), SomeException, catches)
import Foreign.C (CInt (..))
import Lib (run)
import System.Exit (ExitCode (..))
import System.IO (hFlush, hPutStrLn, stderr, stdout)

-- | Bypass GHC finalizers (which can hang on HTTP/2 connection cleanup).
foreign import ccall "exit" c_exit :: CInt -> IO ()

main :: IO ()
main =
    run `catches`
        [ Handler $ \ec -> do
            hFlush stdout
            hFlush stderr
            case (ec :: ExitCode) of
                ExitSuccess -> c_exit 0
                ExitFailure n -> c_exit (fromIntegral n)
        , Handler $ \e -> do
            hPutStrLn stderr $ "Error: " ++ show (e :: SomeException)
            hFlush stderr
            c_exit 1
        ]
