module Main where

import Test.Hspec

import BaselineSpec (baselineSpec)
import BayesianSpec (bayesianSpec)
import ConfigSpec (configSpec)
import Integration (integrationSpec)
import PropertySpec (propertySpec)
import StatsSpec (statsSpec)
import TracingSpec (tracingSpec)
import TUISpec (tuiStateSpec, tuiWidgetsSpec)
import VerifySpec (verifySpec)

main :: IO ()
main = hspec $ do
    statsSpec
    bayesianSpec
    verifySpec
    configSpec
    tracingSpec
    baselineSpec
    tuiStateSpec
    tuiWidgetsSpec
    propertySpec
    integrationSpec
