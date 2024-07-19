{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Use second" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- Options
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Aya.Registration.Core.ENOPNFT.MonetaryPolicy.OnChain (
  mkMoneterayPolicyFunction,
  mkUntypedMintingPolicyFunction,
  MonetaryPolicySettings (..),
  Action (..),
) where

import PlutusTx
import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Prelude qualified

import Adapter.Plutus.OnChain (
  Quantity (..),
  fromMaybe',
  getUniqueTokenNameAndQuantity,
  propertyViolation,
  propertyViolationIfFalse,
  tokenNameGivenToUniqueAndOnlySigner,
 )
import Aya.Registration.Core.Property.Datum.Register
import Aya.Registration.Core.Property.NFT.Transitivity.Register
import Aya.Registration.Core.Validator.OnChain
import Plutus.Script.Utils.Scripts (ValidatorHash (..))

import Aya.Registration.Core.Property.NFT.Ownership.Deregister (
  v_d_2_0_1_Burn_Without_A_Proper_Registration_Validator_Input,
 )
import Aya.Registration.Core.Property.NFT.Ownership.Register (mk_r_Ownerhip_ENOP_NFT)
import Aya.Registration.Core.Property.NFT.Transitivity.Deregister (mk_d_EN_NFT_msgs)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts (ownCurrencySymbol)

data MonetaryPolicySettings = MonetaryPolicySettings
  { ennftCurrencySymbol :: CurrencySymbol
  , registrationValidatorHash :: ScriptHash
  }
  deriving (Prelude.Show, Generic, Ord)

instance Eq MonetaryPolicySettings where
  {-# INLINEABLE (==) #-}
  MonetaryPolicySettings x y == MonetaryPolicySettings x' y' = x == x' && y == y'

instance Eq ValidatorHash where
  {-# INLINEABLE (==) #-}
  ValidatorHash x == ValidatorHash y = x == y

PlutusTx.unstableMakeIsData ''MonetaryPolicySettings
PlutusTx.makeLift ''MonetaryPolicySettings

data Action = Mint | Burn
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Ord)

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Mint == Mint = True
  Burn == Burn = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''Action [('Mint, 0), ('Burn, 1)]
PlutusTx.makeLift ''Action

{-# INLINEABLE mkMoneterayPolicyFunction #-}
mkMoneterayPolicyFunction :: MonetaryPolicySettings -> Action -> ScriptContext -> Bool
mkMoneterayPolicyFunction sp Mint ctx = canMintENOPNFT sp (ownCurrencySymbol ctx) (scriptContextTxInfo ctx)
mkMoneterayPolicyFunction sp Burn ctx = canBurnENOPNFT sp ctx

{-# INLINEABLE mkUntypedMintingPolicyFunction #-}
mkUntypedMintingPolicyFunction :: MonetaryPolicySettings -> BuiltinData -> BuiltinData -> ()
mkUntypedMintingPolicyFunction settings r c =
  check
    ( mkMoneterayPolicyFunction
        settings
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    )

{-# INLINEABLE canMintENOPNFT #-}
canMintENOPNFT :: MonetaryPolicySettings -> CurrencySymbol -> TxInfo -> Bool
canMintENOPNFT MonetaryPolicySettings{..} enopNFTCurrencySymbol txInfo =
  canMintENOPNFT' enopNFTCurrencySymbol txInfo
    $ getRegistrationDatumAndENNFTTokenNameOutput
      registrationValidatorHash
      (getENNFTTokenName mk_r_EN_NFT_msgs ennftCurrencySymbol)
      txInfo

{-# INLINEABLE canMintENOPNFT' #-}
canMintENOPNFT' :: CurrencySymbol -> TxInfo -> (RegistrationDatum, TokenName) -> Bool
canMintENOPNFT' enopNFTCurrencySymbolUsedForMinting txInfo (registrationDatum, ennftTokenNameAsInputValue) =
  mustHaveSameENOPAndENTokenNames
    && mustHaveValidRegistrationDatumDetails
    && mustHaveAnAuthenticRegistrationDatum
    && mustOutputENOPNFTToOperator
  where
    mustHaveSameENOPAndENTokenNames =
      propertyViolationIfFalse
        v_r_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName
        (ennftTokenNameAsInputValue == getENOPNFTTokenName mk_r_ENOP_NFT_msgs enopNFTCurrencySymbolUsedForMinting txInfo)
    mustHaveValidRegistrationDatumDetails =
      propertyViolationIfFalse
        v_3_0_1_o_Registration_Datum_Field_ENNFT_TokenName_Not_Valid
        (ennftTokenNameAsInputValue == ennftTokenName registrationDatum)
        && propertyViolationIfFalse
          v_3_0_2_o_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid
          (enopNFTCurrencySymbolUsedForMinting == enopNFTCurrencySymbol registrationDatum)
    mustHaveAnAuthenticRegistrationDatum =
      propertyViolationIfFalse
        v_3_2_o_Registration_Validator_Output_Datum_Not_Authentic
        (checkRegistrationSignature registrationDatum)
    mustOutputENOPNFTToOperator =
      propertyViolationIfFalse
        v_r_1_1_1_ENOP_NFT_Cardinality_Above_1
        ( ennftTokenNameAsInputValue
            == tokenNameGivenToUniqueAndOnlySigner mk_r_Ownerhip_ENOP_NFT enopNFTCurrencySymbolUsedForMinting txInfo
        )

{-# INLINEABLE canBurnENOPNFT #-}

-- | Check if the ENOP NFT can be burnt by only checking if the Tx references a corresponding Registration validator Input
-- | Deregistering Rules are implemented at the Registration Validator Level
canBurnENOPNFT :: MonetaryPolicySettings -> ScriptContext -> Bool
canBurnENOPNFT = hasARegistratedENNFTInScriptAsInput

{-# INLINEABLE hasARegistratedENNFTInScriptAsInput #-}
hasARegistratedENNFTInScriptAsInput :: MonetaryPolicySettings -> ScriptContext -> Bool
hasARegistratedENNFTInScriptAsInput MonetaryPolicySettings{ennftCurrencySymbol, registrationValidatorHash} =
  ( \case
      (_, One) -> True -- means there is a ENNFT in the Registration script as input
      _ -> propertyViolation v_d_2_0_1_Burn_Without_A_Proper_Registration_Validator_Input
  )
    . getUniqueTokenNameAndQuantity mk_d_EN_NFT_msgs ennftCurrencySymbol
    . ( \case
          TxOut
            { txOutAddress = Address (ScriptCredential _) _
            , txOutValue = v
            , txOutReferenceScript = Nothing
            } -> v
          _ -> propertyViolation v_d_2_0_1_Burn_Without_A_Proper_Registration_Validator_Input
      )
    . txInInfoResolved
    . fromMaybe' (propertyViolation v_d_2_0_1_Burn_Without_A_Proper_Registration_Validator_Input)
    . findRegistrationValidatorInput registrationValidatorHash

{-# INLINEABLE findRegistrationValidatorInput #-}
findRegistrationValidatorInput :: ScriptHash -> ScriptContext -> Maybe TxInInfo
findRegistrationValidatorInput
  registrationValidatorHash
  ScriptContext
    { scriptContextTxInfo = TxInfo{txInfoInputs}
    } =
    find
      ( \case
          TxInInfo{txInInfoResolved = TxOut{txOutAddress = Address (ScriptCredential givenScriptHash) _}} -> givenScriptHash == registrationValidatorHash
          _ -> False
      )
      txInfoInputs
