{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Test.Tasty.Hedgehog.Coverage
  ( Cover
  , testPropertyCoverage
  , withCoverage
  -- * Coverage functions
  , classify
  ) where

import           Data.Typeable                 (Proxy (..))
import           GHC.Stack                     (HasCallStack,
                                                withFrozenCallStack)

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.State           (MonadState, StateT (..), modify,
                                                runStateT)
import           Data.Maybe                    (fromMaybe)
import Data.Monoid ((<>))
import           Data.Map                      (Map)
import qualified Data.Map                      as Map

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
import           Hedgehog.Internal.Report      (Progress (..), Report (..), TestCount (..), ShrinkCount (..),
                                                Result (..))
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

newtype Tally = Tally (Map Text Int)
  deriving (Eq, Show)

data CoveredProperty = CoveredProperty
  { _coverName :: PropertyName
  , _coverProp :: Cover
  }

data Cover = Cover
  { _coverageConf :: !PropertyConfig
  , _coverageProp :: PropertyT (StateT Tally IO) ()
  }

classify :: MonadState Tally m => Bool -> Text -> m ()
classify b l = if b
  then modify (\(Tally t) -> Tally $ Map.alter (Just . maybe 1 (+1)) l t)
  else modify (\(Tally t) -> Tally $ Map.alter (Just . fromMaybe 0) l t)

withCoverage :: HasCallStack => PropertyT (StateT Tally IO) () -> Cover
withCoverage = Cover defaultConfig . withFrozenCallStack . evalM

testPropertyCoverage :: T.TestName -> Cover -> T.TestTree
testPropertyCoverage name cov = T.singleTest name (CoveredProperty (PropertyName name) cov)

prettyTally
  :: PropertyConfig
  -> Report Result
  -> Tally
  -> Doc a
prettyTally _config report (Tally tally) =
  let
    TestCount testCount = reportTests report

    shrinkCount = case reportStatus report of
      Failed fr -> case Report.failureShrinks fr of
                     -- To account for the failed test that triggered the shrink?
                     ShrinkCount n -> 1+n
      _ -> 0

    prettyPct = PP.text . printf "%.2f%%"

    ntests = shrinkCount + testCount

    ratio :: Integral n => n -> Int -> Double
    ratio x y = 100 * (fromIntegral x / fromIntegral y)

    ppTally (l,t) =
      prettyPct (ratio t ntests) <+> PP.text (Text.unpack l)
  in
    PP.vsep $ ppTally <$> Map.toList tally

reportToProgress
  :: PropertyConfig
  -> Report Progress
  -> T.Progress
reportToProgress config (Report testsDone _ status) =
  let
    TestLimit testLimit = propertyTestLimit config
    ShrinkLimit shrinkLimit = propertyShrinkLimit config
    ratio x y = 1.0 * fromIntegral x / fromIntegral y
  in
    -- TODO add details for tests run / discarded / shrunk
    case status of
      Running ->
        T.Progress "Running" (ratio testsDone testLimit)
      Shrinking fr ->
        T.Progress "Shrinking" (ratio (Report.failureShrinks fr) shrinkLimit)

reportOutput
  :: PropertyConfig
  -> Bool
  -> String
  -> Tally
  -> Report Result
  -> IO String
reportOutput config showReplay name tally report@(Report _ _ status) = do
  -- TODO add details for tests run / discarded / shrunk
  rpt <- Report.ppResult (Just (PropertyName name)) report

  let
    toStr = PP.display . PP.renderPrettyDefault
    tal = prettyTally config report tally

  pure $ case status of
    Failed fr -> do
      let
        size = Report.failureSize fr
        seed = Report.failureSeed fr

        replayStr = if showReplay
          then PP.text "Use"
               <+> PP.squotes ("--hedgehog-replay" <+> PP.dquotes (PP.text (show size) <+> PP.text (show seed)))
               <+> "to reproduce"
          else mempty

      toStr $ PP.align tal </> PP.line
        <#> rpt
        <#> replayStr

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
