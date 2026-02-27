module Main where

import Test.Hspec

import AuthSpec (authSpec)
import BaselineSpec (baselineSpec)
import BayesianSpec (bayesianSpec)
import CISpec (ciSpec)
import ConfigSpec (configSpec)
import Integration (integrationSpec)
import MarkdownSpec (markdownSpec)
import OutputSpec (outputSpec)
import PropertySpec (propertySpec)
import StatsSpec (statsSpec)
import TUISpec (tuiStateSpec, tuiWidgetsSpec)
import TracingClientSpec (tracingClientSpec)
import TracingQuerySpec (tracingQuerySpec)
import TracingSpec (tracingSpec)
import ValidationSpec (validationSpec)
import VerifySpec (verifySpec)

main :: IO ()
main = hspec $ do
  statsSpec
  bayesianSpec
  verifySpec
  validationSpec
  configSpec
  tracingSpec
  baselineSpec
  tuiStateSpec
  tuiWidgetsSpec
  propertySpec
  markdownSpec
  tracingQuerySpec
  authSpec
  ciSpec
  outputSpec
  tracingClientSpec
  integrationSpec
