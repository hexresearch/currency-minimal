module Data.Currency.Minimal.MinimalSpec (spec) where

import Test.Hspec
import Test.Hspec.Expectations

import Data.Text (Text)
import Data.Fixed

import Data.Currency.Minimal

spec :: Spec
spec = do
  describe "whatever" $ do
    it "does some basic currency conversions" $ do

      let  exch     = fromList @Text [(("USD","RUB"),72.93)]
      let  usd100   = ("USD", 100) :: (Text, Fixed E2)

      let  rubFromUsd100   = applyRate @Text "RUB" 72.93 usd100
      rubFromUsd100 `shouldBe` ("RUB",7293.00)

      let usdFromRubFromUsd100 = convertMaybe exch "USD" rubFromUsd100
      usdFromRubFromUsd100 `shouldBe` (Just usd100)

      convertMaybe exch "USD" usd100 `shouldBe` (Just usd100)

      pure ()
