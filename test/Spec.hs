module Main where

import Test.Tasty
import Test.Tasty.Runners (NumThreads (..))

import AuthSpec (authSpec)
import BaselineSpec (baselineSpec)
import BayesianSpec (bayesianSpec)
import CISpec (ciSpec)
import CLISpec (cliSpec)
import ConfigSpec (configSpec)
import ContextSpec (contextSpec)
import EnvSpec (envSpec)
import EnvironmentSpec (environmentSpec)
import FrequentistSpec (frequentistSpec)
import Integration (integrationSpec)
import LogSpec (logSpec)
import MarkdownSpec (markdownSpec)
import NwayIntegrationSpec (nwayIntegrationSpec)
import NwaySpec (nwaySpec)
import OutputSpec (outputSpec)
import PropertySpec (propertySpec)
import ReportSpec (reportSpec)
import RunnerBaselineSpec (runnerBaselineSpec)
import StatsCommonSpec (statsCommonSpec)
import StatsSpec (statsSpec)
import TUISpec (tuiStateSpec, tuiWidgetsSpec)
import TracingClientSpec (tracingClientSpec)
import TracingQuerySpec (tracingQuerySpec)
import TracingReportSpec (tracingReportSpec)
import TracingSpec (tracingSpec)
import TypesJsonSpec (typesJsonSpec)
import TypesSpec (typesSpec)
import ValidationSpec (validationSpec)
import VerifySpec (verifySpec)
import WarmupSpec (warmupSpec)

main :: IO ()
main =
  defaultMain $
    localOption (NumThreads 1) $
      testGroup
        "gauntlet"
        [ statsSpec
        , statsCommonSpec
        , bayesianSpec
        , frequentistSpec
        , verifySpec
        , validationSpec
        , configSpec
        , envSpec
        , tracingSpec
        , baselineSpec
        , tuiStateSpec
        , tuiWidgetsSpec
        , propertySpec
        , markdownSpec
        , tracingQuerySpec
        , tracingReportSpec
        , authSpec
        , ciSpec
        , cliSpec
        , outputSpec
        , logSpec
        , contextSpec
        , tracingClientSpec
        , integrationSpec
        , nwaySpec
        , nwayIntegrationSpec
        , typesSpec
        , reportSpec
        , typesJsonSpec
        , environmentSpec
        , warmupSpec
        , runnerBaselineSpec
        ]
