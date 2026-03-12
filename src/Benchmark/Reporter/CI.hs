-- | Reporter backend for CI artifact generation.
module Benchmark.Reporter.CI (ciReporter) where

import Benchmark.Report.CI
  ( CIMode (..)
  , detectCIMode
  , formatForCI
  , writeArtifactReport
  , writeGitHubStepSummary
  )
import Benchmark.Reporter (Reporter (..), noOpReporter)
import Data.Time.Clock.POSIX (getPOSIXTime)

ciReporter :: IO Reporter
ciReporter = do
  ciMode <- detectCIMode
  pure $ case ciMode of
    None -> noOpReporter
    mode ->
      noOpReporter
        { reportRegression = \regression -> do
            formatForCI mode regression
            epoch <- (round <$> getPOSIXTime) :: IO Int
            let reportFile = "results/benchmark-report-" <> show epoch <> ".md"
            writeArtifactReport reportFile regression
            case mode of
              GitHub -> writeGitHubStepSummary regression
              _ -> pure ()
        }
