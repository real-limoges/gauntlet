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
import Integration (integrationSpec)
import LoadControlIntegrationSpec (loadControlIntegrationSpec)
import LogSpec (logSpec)
import MarkdownSpec (markdownSpec)
import NwayIntegrationSpec (nwayIntegrationSpec)
import NwaySpec (nwaySpec)
import OutputSpec (outputSpec)
import PropertySpec (propertySpec)
import RateLimiterSpec (rateLimiterSpec)
import ReportSpec (reportSpec)
import ReporterSpec (reporterSpec)
import RequestSpec (requestSpec)
import StatsCommonSpec (statsCommonSpec)
import StatsSpec (statsSpec)
import TUISpec (tuiStateSpec, tuiWidgetsSpec)
import TracingClientSpec (tracingClientSpec)
import TracingReportSpec (tracingReportSpec)
import TracingSpec (tracingSpec)
import TypesJsonSpec (typesJsonSpec)
import TypesSpec (typesSpec)
import ValidationSpec (validationSpec)
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
        , validationSpec
        , configSpec
        , envSpec
        , tracingSpec
        , baselineSpec
        , tuiStateSpec
        , tuiWidgetsSpec
        , propertySpec
        , markdownSpec
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
        , reporterSpec
        , requestSpec
        , typesJsonSpec
        , environmentSpec
        , warmupSpec
        , rateLimiterSpec
        , loadControlIntegrationSpec
        ]
