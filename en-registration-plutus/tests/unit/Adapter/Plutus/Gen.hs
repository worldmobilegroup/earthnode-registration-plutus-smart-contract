{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}

module Adapter.Plutus.Gen (anyCurrencySymbol, anyENNFTTokenName, anyENNFTTokenNames, anyENNFT, anyAtleast2ENNFTWithSameCurrency, genByteStringOf)
where

import Test.Tasty.QuickCheck (
  Gen,
  chooseInteger,
  listOf1,
  vector,
 )

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NL
import Data.String (IsString (fromString))

import Adapter.Cooked ()
import PlutusLedgerApi.V3 (
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
  toBuiltin,
 )
import Specs.Aya.Registration.Core.Model

anyCurrencySymbol :: Gen CurrencySymbol
anyCurrencySymbol =
  CurrencySymbol
    <$> fmap toBuiltin (genByteStringOf 28)

genByteStringOf :: Int -> Gen ByteString
genByteStringOf n =
  BS.pack <$> vector n

anyENNFTTokenName :: Gen TokenName
anyENNFTTokenName = TokenName . fromString . show <$> chooseInteger (1, 1_0000)

anyENNFTTokenNames :: Gen [TokenName]
anyENNFTTokenNames = listOf1 (TokenName . fromString . show <$> chooseInteger (1, 1_0000))

anyENNFT :: Gen ENNFT
anyENNFT = NFT <$> anyCurrencySymbol <*> anyENNFTTokenName

anyAtleast2ENNFTWithSameCurrency :: Gen (CurrencySymbol, TokenName, NL.NonEmpty TokenName)
anyAtleast2ENNFTWithSameCurrency = do
  givenCurrencySymbol <- anyCurrencySymbol
  tn1s <- listOf1 (TokenName . fromString . show <$> chooseInteger (1, 500))
  tn2s <- listOf1 (TokenName . fromString . show <$> chooseInteger (500, 1_000))
  pure (givenCurrencySymbol, head tn1s, NL.fromList tn2s)
