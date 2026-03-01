module TastyCompat
  ( shouldBe
  , shouldSatisfy
  , shouldReturn
  , shouldContain
  , shouldNotContain
  , Expectation
  )
where

import Data.List (isInfixOf)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure)

type Expectation = IO ()

shouldBe :: (Eq a, Show a) => a -> a -> IO ()
shouldBe actual expected = assertEqual "" expected actual

shouldSatisfy :: Show a => a -> (a -> Bool) -> IO ()
shouldSatisfy val p
  | p val = pure ()
  | otherwise = assertFailure $ "Predicate failed on: " ++ show val

shouldReturn :: (Eq a, Show a) => IO a -> a -> IO ()
shouldReturn action expected = action >>= (`shouldBe` expected)

shouldContain :: (Eq a, Show a) => [a] -> [a] -> IO ()
shouldContain haystack needle
  | needle `isInfixOf` haystack = pure ()
  | otherwise =
      assertBool
        ("Expected " ++ show haystack ++ " to contain " ++ show needle)
        False

shouldNotContain :: (Eq a, Show a) => [a] -> [a] -> IO ()
shouldNotContain haystack needle
  | not (needle `isInfixOf` haystack) = pure ()
  | otherwise =
      assertBool
        ("Expected " ++ show haystack ++ " to NOT contain " ++ show needle)
        False
