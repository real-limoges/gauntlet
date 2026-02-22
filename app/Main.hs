module Main where

import Control.Exception (SomeException, catches, Handler (..))
import Foreign.C (CInt (..))
import Lib (run)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (hPutStrLn, stderr)

foreign import ccall "exit" c_exit :: CInt -> IO ()

main :: IO ()
main =
    run `catches`
        [ Handler $ \ec -> case (ec :: ExitCode) of
            ExitSuccess -> pure ()
            ExitFailure n -> c_exit (fromIntegral n)
        , Handler $ \e -> do
            hPutStrLn stderr $ "Error: " ++ show (e :: SomeException)
            exitFailure
        ]
