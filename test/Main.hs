{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.Hedgehog.Coverage

genAlphaList :: Gen String
genAlphaList =
  Gen.list (Range.linear 0 100) Gen.alpha

test_involutive :: (MonadTest m, Eq a, Show a) => (a -> a) -> a -> m ()
test_involutive f x =
  f (f x) === x

prop_reverse_involutive :: Cover
prop_reverse_involutive = withCoverage $ do
  xs <- forAll genAlphaList
  classify (length xs > 50) "non-trivial"
  test_involutive reverse xs

badReverse :: [a] -> [a]
badReverse []  = []
badReverse [_] = []
badReverse as  = reverse as

prop_badReverse_involutive :: Cover
prop_badReverse_involutive = withCoverage $ do
  xs <- forAll genAlphaList
  classify (length xs < 50) "non-trivial"
  test_involutive badReverse xs

main :: IO ()
main = defaultMain $
  testGroup "tasty-hedgehog-coverage tests"
    [ testPropertyCoverage
        "reverse involutive"
        prop_reverse_involutive
    , expectFail $ testPropertyCoverage
        "badReverse involutive fails"
         prop_badReverse_involutive
    ]
