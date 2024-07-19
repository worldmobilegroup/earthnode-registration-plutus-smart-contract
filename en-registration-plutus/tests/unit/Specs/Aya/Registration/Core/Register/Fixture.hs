{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Specs.Aya.Registration.Core.Register.Fixture (
  FixtureNominalCase (..),
  genFixtureNominalCase,
  FixtureENNFTWithInvalidQuantityAbove1 (..),
  genFixtureENNFTWithWrongQuantityAbove1,
  FixtureMultipleENNFTs (..),
  genFixtureMultipleENNFTs,
  FixtureWithInvalidSignature (..),
  genFixtureWithInvalidSignature,
) where

import Cooked (InitialDistribution (..), distributionFromList, walletPKHash)

import Test.Tasty.QuickCheck (
  Gen,
  chooseInteger,
  elements,
  suchThat,
 )

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair (..), sign)
import Cooked.ValueUtils (ada)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.List.NonEmpty qualified as NL

import Adapter.Cooked ()
import Adapter.Plutus.Gen (anyAtleast2ENNFTWithSameCurrency, anyCurrencySymbol, anyENNFT, genByteStringOf)
import Aya.Registration.Core.Validator.Builder
import Aya.Registration.Core.Validator.OnChain
import PlutusLedgerApi.V3 (
  CurrencySymbol (CurrencySymbol),
  TokenName,
  fromBuiltin,
  singleton,
 )
import Specs.Aya.Registration.Core.Model (
  ENNFT,
  NFT (..),
  anotherOperator,
  nftToValue,
  operator,
 )

data FixtureNominalCase a = FixtureNominalCase
  { genesis :: InitialDistribution
  , substrateKeyPair :: KeyPair a
  , commission :: Integer
  , ennft :: ENNFT
  }
instance Show (FixtureNominalCase a) where
  show FixtureNominalCase{..} = "FixtureNominalCase { commission = " <> show commission <> ", ennft = " <> show ennft <> " }"

data FixtureWithInvalidSignature a = FixtureWithInvalidSignature
  { genesis :: InitialDistribution
  , substrateKeyPair :: KeyPair a
  , commission :: Integer
  , ennft :: ENNFT
  , invalidSignature :: ByteString
  }
  deriving (Show)

data FixtureENNFTWithInvalidQuantityAbove1 a = FixtureENNFTWithInvalidQuantityAbove1
  { genesis :: InitialDistribution
  , substrateKeyPair :: KeyPair a
  , commission :: Integer
  , ennft :: ENNFT
  , wrongENNFTQuantity :: Integer
  }
  deriving (Show)

data FixtureMultipleENNFTs a = FixtureMultipleENNFTs
  { genesis :: InitialDistribution
  , substrateKeyPair :: KeyPair a
  , commission :: Integer
  , ennftCurrencySymbol :: CurrencySymbol
  , firstEnnftTn :: TokenName
  , ennftsTokenNames :: NL.NonEmpty TokenName
  , anotherDifferentENNftCurrencySymbol :: CurrencySymbol
  }

instance Show (FixtureMultipleENNFTs a) where
  show :: FixtureMultipleENNFTs a -> String
  show FixtureMultipleENNFTs{..} =
    "FixtureMultipleENNFTs { commission = "
      <> show commission
      <> ", ennftCurrencySymbol = "
      <> show ennftCurrencySymbol
      <> ", firstEnnftTn = "
      <> show firstEnnftTn
      <> ", ennftsTokenNames = "
      <> show ennftsTokenNames
      <> " }"

genFixtureNominalCase :: [KeyPair a] -> Gen (FixtureNominalCase a)
genFixtureNominalCase keypairs = do
  anEnnft <- anyENNFT
  FixtureNominalCase
    ( distributionFromList
        [
          ( operator
          ,
            [ ada 500
            , ada 5
            , ada 5
            , ada 5
            , ada 2 <> nftToValue anEnnft
            ]
          )
        ,
          ( anotherOperator
          ,
            [ ada 500
            , ada 5
            , ada 5
            , ada 5
            ]
          )
        ]
    )
    <$> elements keypairs
    <*> chooseInteger (0, 100)
    <*> pure anEnnft

genFixtureENNFTWithWrongQuantityAbove1 :: [KeyPair a] -> Gen (FixtureENNFTWithInvalidQuantityAbove1 a)
genFixtureENNFTWithWrongQuantityAbove1 keypairs = do
  aSubstrateKeyPair <- elements keypairs
  anEnnft <- anyENNFT
  aWrongENNFTQuantity <- chooseInteger (2, 100)
  aCommission <- chooseInteger (0, 100)
  pure
    FixtureENNFTWithInvalidQuantityAbove1
      { genesis =
          distributionFromList
            [
              ( operator
              ,
                [ ada 100
                , ada 5
                , ada 5
                , ada 5
                , ada 5
                , ada 2 <> singleton (currencySymbol anEnnft) (tokenName anEnnft) aWrongENNFTQuantity
                ]
              )
            ]
      , substrateKeyPair = aSubstrateKeyPair
      , commission = aCommission
      , ennft = anEnnft
      , wrongENNFTQuantity = aWrongENNFTQuantity
      }

genFixtureMultipleENNFTs :: [KeyPair a] -> Gen (FixtureMultipleENNFTs a)
genFixtureMultipleENNFTs keypairs = do
  aSubstrateKeyPair <- elements keypairs
  (aCurrencySymbol, aFirstEnnftTn, anEnnftsTokenNames) <- anyAtleast2ENNFTWithSameCurrency
  anotherDifferentENNftCurrencySymbol <- anyCurrencySymbol `suchThat` (/= aCurrencySymbol)
  aCommission <- chooseInteger (0, 100)
  pure
    FixtureMultipleENNFTs
      { genesis =
          distributionFromList
            [
              ( operator
              ,
                [ ada 100
                , ada 5
                , ada 5
                , ada 5
                , ada 5
                , ada 2 <> foldMap (\tn -> singleton aCurrencySymbol tn 1) (anEnnftsTokenNames <> NL.fromList [aFirstEnnftTn])
                ]
              )
            ]
      , substrateKeyPair = aSubstrateKeyPair
      , commission = aCommission
      , ennftCurrencySymbol = aCurrencySymbol
      , firstEnnftTn = aFirstEnnftTn
      , ennftsTokenNames = anEnnftsTokenNames
      , anotherDifferentENNftCurrencySymbol = anotherDifferentENNftCurrencySymbol
      }

genFixtureWithInvalidSignature
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> Gen (FixtureWithInvalidSignature a)
genFixtureWithInvalidSignature keypairs = do
  aSubstrateKeyPair <- elements keypairs
  anEnnft <- anyENNFT
  aCommission <- chooseInteger (0, 100)
  let settings = coerce . currencySymbol $ anEnnft
      validSignedMessage =
        sign
          (signatureKey aSubstrateKeyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName anEnnft)
                (walletPKHash operator)
                aCommission -- signing with a different commission
                (associatedENOPNFTCurrencySymbol settings)
          )
  anInvalidSignature <- genByteStringOf 64 `suchThat` (/= validSignedMessage)
  pure
    FixtureWithInvalidSignature
      { genesis =
          distributionFromList
            [
              ( operator
              ,
                [ ada 100
                , ada 5
                , ada 5
                , ada 5
                , ada 5
                , ada 2 <> nftToValue anEnnft
                ]
              )
            ]
      , substrateKeyPair = aSubstrateKeyPair
      , commission = aCommission
      , ennft = anEnnft
      , invalidSignature = anInvalidSignature
      }
