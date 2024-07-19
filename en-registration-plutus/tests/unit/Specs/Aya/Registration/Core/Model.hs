{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Specs.Aya.Registration.Core.Model (
  ENNFT,
  ENOPNFT,
  NFT (..),
  Commission,
  Quantity,
  nfts,
  nftToValue,
  operator,
  anotherOperator,
  registrationCookedConfig,
) where

import Cooked

import PlutusTx.AssocMap qualified as PMap

import Aya.Registration.Core.ENOPNFT.MonetaryPolicy.OnChain (Action (..))

import Adapter.CardanoCryptoClass.Crypto
import Aya.Registration.Core.Validator.Builder (
  associatedENOPNFTMonetaryPolicy,
  typedRegistrationValidator,
 )
import Aya.Registration.Core.Validator.OnChain (
  RegistrationAction (..),
  RegistrationDatum (..),
  RegistrationValidatorSettings (..),
 )
import Cooked qualified as C
import Data.Default
import Data.Text qualified as Text
import PlutusLedgerApi.V3 (CurrencySymbol, TokenName, Value, fromBuiltin)
import PlutusLedgerApi.V3 qualified as V3
import Prettyprinter.Extras (Pretty (..))

data NFT = NFT {currencySymbol :: CurrencySymbol, tokenName :: TokenName} deriving (Eq, Show)
type ENNFT = NFT
type ENOPNFT = NFT
type Commission = Integer
type Quantity = Integer

-- | Make a 'Value' containing only the given quantity of the given currency.
nfts :: CurrencySymbol -> [TokenName] -> V3.Value
nfts c tns = V3.Value $ PMap.singleton c (PMap.fromList $ (,1) <$> tns)

nftToValue :: NFT -> Value
nftToValue NFT{..} = V3.singleton currencySymbol tokenName 1

operator :: Wallet
operator = wallet 1

anotherOperator :: Wallet
anotherOperator = wallet 2

registrationCookedConfig :: CurrencySymbol -> C.PrettyCookedOpts
registrationCookedConfig ennftCurrencySymbol =
  let settings = RegistrationValidatorSettings ennftCurrencySymbol
   in def
        { C.pcOptHashes =
            def
              { C.pcOptHashNames =
                  C.hashNamesFromList
                    [ (operator, "Operator")
                    , (anotherOperator, "Another Operator")
                    ]
                    <> C.hashNamesFromList
                      [ (associatedENOPNFTMonetaryPolicy settings, "ENOP NFT")
                      ]
                    <> C.hashNamesFromList
                      [ (typedRegistrationValidator settings, "Registration Validator")
                      ]
                    <> C.defaultHashNames -- IMPORTANT: must be the last element
              }
        }

instance PrettyCooked Action where
  prettyCookedOpt _ Mint = "Mint ENNOPNFT"
  prettyCookedOpt _ Burn = "Burn ENNOPNFT"

instance PrettyCooked RegistrationAction where
  prettyCookedOpt _ UpdateRegistrationDetails = "UpdateRegistrationDetails"
  prettyCookedOpt _ Deregister = "Deregister"

instance PrettyCooked RegistrationDatum where
  prettyCookedOpt opts RegistrationDatum{..} =
    "RegistrationDatum: "
      <> "\n  ayaValidatorPublicKey: "
      <> pretty (Text.unpack . encode . fromByteString @Hexadecimal . fromBuiltin $ ayaValidatorPublicKey)
      <> "\n  ennftTokenName: "
      <> prettyCookedOpt opts ennftTokenName
      <> "\n  cardanoRewardPubKey: "
      <> prettyCookedOpt opts cardanoRewardPubKey
      <> "\n  commission: "
      <> prettyCookedOpt opts commission
      <> "\n  enopNFTCurrencySymbol: "
      <> prettyCookedOpt opts enopNFTCurrencySymbol
      <> "\n  signature: "
      <> pretty (Text.unpack . encode . fromByteString @Hexadecimal . fromBuiltin $ signature)
