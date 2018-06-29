{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Tasty.Hedgehog.Coverage
  ( Cover
  , testPropertyCoverage
  , withCoverage
  -- * Coverage functions
  , classify
  , label
  , collect
  ) where

import           Data.Typeable                 (Proxy (..))
import           GHC.Stack                     (HasCallStack,
                                                withFrozenCallStack)

import           Control.Monad                 (when)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.State           (MonadState, StateT (..), modify,
                                                runStateT)

import           Data.Map                      (Map)
import qualified Data.Map                      as Map

import           Data.Maybe                    (fromMaybe)

import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import           Hedgehog
import           Hedgehog.Internal.Property    (PropertyConfig (..),
                                                PropertyName (..),
                                                PropertyT (..),
                                                ShrinkLimit (..),
                                                TestLimit (..), defaultConfig,
                                                propertyShrinkLimit,
                                                propertyTestLimit)
import           Hedgehog.Internal.Report      (FailureReport (FailureReport, failureShrinks),
                                                Progress (..), Report (..),
                                                Result (..), ShrinkCount (..),
                                                TestCount (..))
import qualified Hedgehog.Internal.Report      as Report
import           Hedgehog.Internal.Runner      (checkReport)
import qualified Hedgehog.Internal.Seed        as Seed

import           Text.PrettyPrint.Annotated.WL (Doc, (<#>), (<+>), (</>))
import qualified Text.PrettyPrint.Annotated.WL as PP
import           Text.Printf                   (printf)

import           Test.Tasty.Options
import qualified Test.Tasty.Providers          as T

import           Test.Tasty.Hedgehog           (HedgehogDiscardLimit (..),
                                                HedgehogReplay (..),
                                                HedgehogShowReplay (..),
                                                HedgehogShrinkLimit (..),
                                                HedgehogShrinkRetries (..),
                                                HedgehogTestLimit (..))

newtype Tally = Tally
  { unTally :: Map Text Int
  }
  deriving (Eq, Show)

data CoveredProperty = CoveredProperty
  { _coverName :: PropertyName
  , _coverProp :: Cover
  }

data Cover = Cover
  { _coverageConf :: !PropertyConfig
  , _coverageProp :: PropertyT (StateT Tally IO) ()
  }

-- | Records how many test cases satisfy a given condition.
--
-- @
-- prop_reverse_involutive :: Cover
-- prop_reverse_involutive = withCoverage $ do
--   xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
--   classify (length xs > 50) "non-trivial"
--   test_involutive reverse xs
-- @
-- Which gives output similar to:
-- @
--  reverse involutive:          OK
--    18.00% non-trivial
-- @
--
classify
  :: MonadState Tally m
  => Bool                       -- ^ @True@ if this case should be included.
  -> Text                       -- ^ The label for this input.
  -> m ()
classify b l = when b $
  modify (Tally . Map.alter (Just . maybe 1 (+1)) l . unTally)

-- | Attach a simple label to a property.
-- @
-- prop_reverse_reverse :: Cover
-- prop_reverse_reverse = withCoverage $ do
--   xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
--   label ("length of input is " ++ show (length xs))
--   reverse (reverse xs) === xs
-- @
-- Which gives output similar to:
-- @
-- reverse involutive:          OK
--     4.00% with a length of 0
--     7.00% with a length of 1
--     3.00% with a length of 11
--     2.00% with a length of 12
--     2.00% with a length of 13
--     ...
-- @
--
label
  :: MonadState Tally m
  => Text                       -- ^ The label for the input.
  -> m ()
label =
  classify True

-- | Uses the input itself as the label, useful for recording test case distribution.
--
-- @
-- prop_reverse_reverse :: Cover
-- prop_reverse_reverse = withCoverage $ do
--   xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
--   collect (length xs)
--   reverse (reverse xs) === xs
-- @
-- Which gives output similar to:
-- @
-- reverse involutive:          OK
--     8.00% ""
--     1.00% "AFkNJBLiWYEBFRyZhulpMkkqIvsDpLAmaYoFTnNNFfkrbPUqDIRUuZOFGohTfB"
--     1.00% "AWWfLCfmZPoydVYXwnFHyCEWztXanEzdoc"
--     1.00% "CJJVBGOeaIkLfcOUGV"
--     1.00% "CNrTsblqfEz"
--     1.00% "CxDqm"
-- @
--
collect
  :: ( MonadState Tally m
     , Show a
     )
  => a                          -- ^ The input to collect.
  -> m ()
collect =
  label . Text.pack . show

withCoverage
  :: HasCallStack
  => PropertyT (StateT Tally IO) ()
  -> Cover
withCoverage m =
  Cover defaultConfig $ withFrozenCallStack (evalM m)

testPropertyCoverage
  :: T.TestName
  -> Cover
  -> T.TestTree
testPropertyCoverage name cov =
  T.singleTest name (CoveredProperty (PropertyName name) cov)

ratio :: Integral n => n -> Int -> Double
ratio x y = fromIntegral x / fromIntegral y

prettyTally
  :: PropertyConfig
  -> Report Result
  -> Tally
  -> Doc a
prettyTally _config report (Tally tally) =
  let
    TestCount testCount = reportTests report

    shrinkCount = case reportStatus report of
      -- Account for the failed test that might have included our classified case,
      -- otherwise numbers are skewed. I am not convinced this is correct though.
      Failed FailureReport {failureShrinks = ShrinkCount n} -> 1 + n
      -- We haven't had to shrink so there were no test failures so our
      -- testCount is indicative of the total number of tests that were run.
      _                                                     -> 0

    ntests = shrinkCount + testCount

    ppTally (l,t) =
      PP.text (printf "%.2f%%" (100.0 * ratio t ntests)) <+>
      PP.text (Text.unpack l)
  in
    PP.vsep $ ppTally <$> Map.toList tally

reportToProgress
  :: PropertyConfig
  -> Report Progress
  -> T.Progress
reportToProgress config (Report testsDone _ status) =
  let
    TestLimit testLimit     = propertyTestLimit config
    ShrinkLimit shrinkLimit = propertyShrinkLimit config
  in
    case status of
      Running ->
        T.Progress "Running" (1.0 * realToFrac (ratio testsDone testLimit))
      Shrinking fr ->
        T.Progress "Shrinking" (1.0 * realToFrac (ratio (Report.failureShrinks fr) shrinkLimit))

reportOutput
  :: PropertyConfig
  -> Bool
  -> String
  -> Tally
  -> Report Result
  -> IO String
reportOutput config showReplay name tally report@(Report _ _ status) = do
  rpt <- Report.ppResult (Just (PropertyName name)) report

  let
    toStr = PP.display . PP.renderPrettyDefault
    tal = prettyTally config report tally

  pure $ case status of
    Failed fr -> do
      let
        size = PP.text . show $ Report.failureSize fr
        seed = PP.text . show $ Report.failureSeed fr

        replayStr = if showReplay
          then PP.text "Use"
               <+> PP.squotes ("--hedgehog-replay" <+> PP.dquotes (size <+> seed))
               <+> "to reproduce"
          else mempty

      toStr $ PP.align tal </> PP.line <#> rpt <#> replayStr

    GaveUp -> "Gave up"
    OK     -> toStr tal

instance T.IsTest CoveredProperty where
  testOptions =
    return [ Option (Proxy :: Proxy HedgehogReplay)
           , Option (Proxy :: Proxy HedgehogShowReplay)
           , Option (Proxy :: Proxy HedgehogTestLimit)
           , Option (Proxy :: Proxy HedgehogDiscardLimit)
           , Option (Proxy :: Proxy HedgehogShrinkLimit)
           , Option (Proxy :: Proxy HedgehogShrinkRetries)
           ]

  run opts (CoveredProperty name (Cover conf prop)) yieldProgress = do
    let
      HedgehogReplay         replay = lookupOption opts
      HedgehogShowReplay showReplay = lookupOption opts
      HedgehogTestLimit       mTests = lookupOption opts
      HedgehogDiscardLimit mDiscards = lookupOption opts
      HedgehogShrinkLimit   mShrinks = lookupOption opts
      HedgehogShrinkRetries mRetries = lookupOption opts

      config =
        PropertyConfig
          (fromMaybe (propertyTestLimit conf) mTests)
          (fromMaybe (propertyDiscardLimit conf) mDiscards)
          (fromMaybe (propertyShrinkLimit conf) mShrinks)
          (fromMaybe (propertyShrinkRetries conf) mRetries)

    randSeed <- Seed.random

    let
      size = maybe 0 fst replay
      seed = maybe randSeed snd replay

      runProp = checkReport config size seed prop
        (liftIO . yieldProgress . reportToProgress config)

    (rresult, tally) <- runStateT runProp (Tally mempty)

    let
      resultFn = if reportStatus rresult == OK
                 then T.testPassed
                 else T.testFailed

    out <- reportOutput config showReplay (unPropertyName name) tally rresult
    return $ resultFn out
