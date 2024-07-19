{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v2.0
-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Aya.Registration.Core.Validator.OnChain (
  mkUntypedValidatorFunction,
  mkValidatorFunction,
  RegistrationValidatorSettings (..),
  RegistrationDatum (..),
  RegistrationAction (..),
  checkRegistrationSignature,
  mkHashedRegistrationMessage,
  getRegistrationDatumAndENNFTTokenNameOutput,
  getENOPNFTTokenName,
  getENNFTTokenName,
) where

import GHC.Generics (Generic)

import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Prelude as Plutus.Prelude
import Prelude qualified

import PlutusTx.Builtins qualified as Cryptography

import Adapter.Plutus.OnChain
import PlutusTx.Applicative ()

import Aya.Registration.Core.Property.Datum.Register
import Aya.Registration.Core.Property.NFT.Transitivity.Deregister
import Aya.Registration.Core.Property.NFT.Transitivity.Update
import Aya.Registration.Core.Property.Violation
import PlutusTx.AssocMap qualified as Map

newtype RegistrationValidatorSettings = RegistrationValidatorSettings
  { ennftCurrencySymbol :: CurrencySymbol
  }
  deriving (Prelude.Show, Generic, Ord)

instance Eq RegistrationValidatorSettings where
  {-# INLINEABLE (==) #-}
  RegistrationValidatorSettings x == RegistrationValidatorSettings y = x == y

PlutusTx.unstableMakeIsData ''RegistrationValidatorSettings
PlutusTx.makeLift ''RegistrationValidatorSettings

data RegistrationDatum = RegistrationDatum
  { ayaValidatorPublicKey :: BuiltinByteString
  -- ^ Substrate Public Keys of the Aya Validator
  , signature :: BuiltinByteString
  -- ^ Signature of the datum. All datum fields below concatenated and signed by the substrateAVPublicKey
  , ennftTokenName :: TokenName
  -- ^ Unique ENNFT name, "used" for this registration
  , cardanoRewardPubKey :: PubKeyHash
  -- ^ Operator's wallet where rewards will be delivered after participating in a block production in Aya
  , commission :: Integer
  -- ^ Commission in percent shared with staking delegators.
  , enopNFTCurrencySymbol :: CurrencySymbol -- We cannot store the EnOpNft CurrencySymbol in the parameter because we get a cyclic dependency
  }
  deriving (Prelude.Show, Generic, Ord)

instance Eq RegistrationDatum where
  {-# INLINEABLE (==) #-}
  x == y =
    ayaValidatorPublicKey x
      == ayaValidatorPublicKey y
      && ennftTokenName x
      == ennftTokenName y
      && cardanoRewardPubKey x
      == cardanoRewardPubKey y
      && commission x
      == commission y
      && enopNFTCurrencySymbol x
      == enopNFTCurrencySymbol y
      && signature x
      == signature y

PlutusTx.unstableMakeIsData ''RegistrationDatum
PlutusTx.makeLift ''RegistrationDatum

-- | The actions that can be performed by the operator
-- | N.B : The action Register is enforced by the ENOPNFT NFT minting policy
data RegistrationAction
  = -- | Deregister the operator
    Deregister
  | -- | Update the registration information
    UpdateRegistrationDetails
  deriving (Prelude.Show, Generic)

instance Eq RegistrationAction where
  {-# INLINEABLE (==) #-}
  Deregister == Deregister = True
  UpdateRegistrationDetails == UpdateRegistrationDetails = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''RegistrationAction [('UpdateRegistrationDetails, 0), ('Deregister, 1)]
PlutusTx.makeLift ''RegistrationAction

{-# INLINEABLE mkValidatorFunction #-}
mkValidatorFunction :: RegistrationValidatorSettings -> RegistrationDatum -> RegistrationAction -> ScriptContext -> Bool
mkValidatorFunction registrationValidatorSettings _ UpdateRegistrationDetails ctx = canUpdateRegistration registrationValidatorSettings ctx
mkValidatorFunction registrationValidatorSettings datum Deregister ctx = canDeregister registrationValidatorSettings datum ctx

{-# INLINEABLE mkUntypedValidatorFunction #-}
mkUntypedValidatorFunction :: RegistrationValidatorSettings -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedValidatorFunction s d r c =
  check
    ( mkValidatorFunction
        s
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    )

{-# INLINEABLE canUpdateRegistration #-}
canUpdateRegistration :: RegistrationValidatorSettings -> ScriptContext -> Bool
canUpdateRegistration RegistrationValidatorSettings{..} ctx =
  let getENNFTTokenNameForUpdate = getENNFTTokenName mk_u_EN_NFT_msgs ennftCurrencySymbol
      (datumInput, ennftTokenNameInput) = getRegistrationDatumAndENNFTTokenNameInput getENNFTTokenNameForUpdate ctx
      (datumOutput, ennftTokenNameOutput) = getRegistrationDatumAndENNFTTokenNameOutput (ownHash ctx) getENNFTTokenNameForUpdate (scriptContextTxInfo ctx)
      enopNFTNameOutput = tokenNameGivenToUniqueAndOnlySigner mk_u_ENOP_NFT_msgs (enopNFTCurrencySymbol datumInput) (scriptContextTxInfo ctx)
      enopNFTNameInput = tokenNameSpent mk_u_ENOP_NFT_msgs (enopNFTCurrencySymbol datumInput) (scriptContextTxInfo ctx)
   in shouldNotMintOrBurn (scriptContextTxInfo ctx)
        && propertyViolationIfFalse
          v_u_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName
          (enopNFTNameOutput == ennftTokenNameOutput)
        && propertyViolationIfFalse
          v_3_2_i_Registration_Validator_Input_Datum_Not_Authentic
          (checkRegistrationSignature datumInput)
        && propertyViolationIfFalse
          v_3_2_o_Registration_Validator_Output_Datum_Not_Authentic
          (checkRegistrationSignature datumOutput)
        && propertyViolationIfFalse
          v_3_0_1_o_Registration_Datum_Field_ENNFT_TokenName_Not_Valid
          ( ennftTokenName datumInput
              == ennftTokenName datumOutput
              && ennftTokenNameInput
              == ennftTokenNameOutput
              && ennftTokenNameOutput
              == ennftTokenName datumOutput
          )
        && propertyViolationIfFalse
          v_3_0_2_o_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid
          ( enopNFTCurrencySymbol datumInput
              == enopNFTCurrencySymbol datumOutput
              && enopNFTNameInput
              == enopNFTNameOutput
              && enopNFTNameOutput
              == ennftTokenName datumOutput
          )

{-# INLINEABLE shouldNotMintOrBurn #-}
shouldNotMintOrBurn :: TxInfo -> Bool
shouldNotMintOrBurn txInfo =
  propertyViolationIfFalse v_u_1_0_2_No_Minting_Allowed (not . hasAnyPositiveQuantities . txInfoMint $ txInfo)
    && propertyViolationIfFalse v_u_1_0_3_No_Burning_Allowed (not . hasAnyNegativeQuantity . txInfoMint $ txInfo)

{-# INLINEABLE checkRegistrationSignature #-}
-- {-# INLINABLE checkDatumSig #-}
-- Check the signature of the registration datum
checkRegistrationSignature :: RegistrationDatum -> Bool
checkRegistrationSignature RegistrationDatum{..} =
  Cryptography.verifyEd25519Signature
    ayaValidatorPublicKey
    (mkHashedRegistrationMessage ennftTokenName cardanoRewardPubKey commission enopNFTCurrencySymbol)
    signature

{-# INLINEABLE mkHashedRegistrationMessage #-}
mkHashedRegistrationMessage :: TokenName -> PubKeyHash -> Integer -> CurrencySymbol -> BuiltinByteString
mkHashedRegistrationMessage ennftTokenName cardanoRewardPubKey commission enopNFTCurrencySymbol =
  Cryptography.blake2b_256
    . appendByteString (unTokenName ennftTokenName)
    . appendByteString (getPubKeyHash cardanoRewardPubKey)
    . consByteString commission
    $ unCurrencySymbol enopNFTCurrencySymbol

{-# INLINEABLE getRegistrationDatumAndENNFTTokenNameOutput #-}
getRegistrationDatumAndENNFTTokenNameOutput
  :: ScriptHash -> (Value -> TokenName) -> TxInfo -> (RegistrationDatum, TokenName)
getRegistrationDatumAndENNFTTokenNameOutput registrationValidatorHash getENNFTTokenName' =
  \case
    [] -> propertyViolation v_3_3_o_Registration_Validator_Output_NotFound
    [(OutputDatum (Datum scriptDatum), scriptValue)] ->
      ( fromMaybe' (propertyViolation v_3_1_o_Registration_Output_Datum_Not_Deserializable) . deserializeDatum $ scriptDatum
      , getENNFTTokenName' scriptValue
      )
    [(NoOutputDatum, _)] -> propertyViolation v_3_4_o_Registration_Validator_Output_Has_No_Datum
    [(OutputDatumHash _, _)] -> propertyViolation v_3_5_o_Registration_Validator_Output_Has_Only_Hashed_Datum
    _ -> propertyViolation v_3_6_o_More_Than_1_Registration_Validator_Output
    . scriptOutputsAt registrationValidatorHash

{-# INLINEABLE getRegistrationDatumAndENNFTTokenNameInput #-}
getRegistrationDatumAndENNFTTokenNameInput :: (Value -> TokenName) -> ScriptContext -> (RegistrationDatum, TokenName)
getRegistrationDatumAndENNFTTokenNameInput getENNFTTokenName' =
  \case
    (OutputDatum (Datum scriptDatum), scriptValue) ->
      ( fromMaybe' (propertyViolation v_3_1_i_Registration_Input_Datum_Not_Deserializable) . deserializeDatum $ scriptDatum
      , getENNFTTokenName' scriptValue
      )
    (NoOutputDatum, _) -> propertyViolation v_3_4_i_Registration_Validator_Input_Has_No_Datum
    (OutputDatumHash _, _) -> propertyViolation v_3_5_i_Registration_Validator_Input_Has_Only_Hashed_Datum
    . ( \case
          TxOut
            { txOutAddress = Address (ScriptCredential _) _
            , txOutValue = v
            , txOutDatum = d
            , txOutReferenceScript = Nothing
            } -> (d, v)
          _ -> propertyViolation v_3_3_i_Registration_Validator_Input_NotFound
      )
    . txInInfoResolved
    . fromMaybe' (propertyViolation v_3_3_i_Registration_Validator_Input_NotFound)
    . findOwnInput

{-# INLINEABLE deserializeDatum #-}
deserializeDatum :: BuiltinData -> Maybe RegistrationDatum
deserializeDatum = fromBuiltinData

{-# INLINEABLE getENOPNFTTokenName #-}
getENOPNFTTokenName :: NFTPropertyViolationMsg -> CurrencySymbol -> TxInfo -> TokenName
getENOPNFTTokenName msgs enopNFTCurrencySymbol =
  getNFTTokenName
    msgs
    enopNFTCurrencySymbol
    . txInfoMint

{-# INLINEABLE getENNFTTokenName #-}
getENNFTTokenName :: NFTPropertyViolationMsg -> CurrencySymbol -> Value -> TokenName
getENNFTTokenName = getNFTTokenName

{-# INLINEABLE canDeregister #-}
canDeregister :: RegistrationValidatorSettings -> RegistrationDatum -> ScriptContext -> Bool
canDeregister RegistrationValidatorSettings{..} RegistrationDatum{..} ctx =
  ( \enopNFTToBurn ->
      let getENNFTTokenNameForUpdate = getENNFTTokenName mk_d_EN_NFT_msgs ennftCurrencySymbol
          ennftTokenNameInput = snd $ getRegistrationDatumAndENNFTTokenNameInput getENNFTTokenNameForUpdate ctx
          nftsWithSameTokenNames =
            propertyViolationIfFalse
              v_d_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName
              (ennftTokenNameInput == enopNFTToBurn)
          hasENNFTReleasedToOperator =
            propertyViolationIfFalse
              v_d_1_0_3_No_ENNFT_Released_To_Operator
              (tokenNameGivenToUniqueAndOnlySigner mk_d_EN_NFT_msgs ennftCurrencySymbol (scriptContextTxInfo ctx) == ennftTokenName)
       in nftsWithSameTokenNames
            && hasENNFTReleasedToOperator
  )
    . getENOPNftToBurn ctx
    $ enopNFTCurrencySymbol

{-# INLINEABLE getENOPNftToBurn #-}
getENOPNftToBurn :: ScriptContext -> CurrencySymbol -> TokenName
getENOPNftToBurn ctx =
  ( \case
      [] -> propertyViolation v_d_1_0_0_No_ENNOP_To_Burn
      [(enopNFTToBurn, burnQuantity)] ->
        if burnQuantity == -1
          then enopNFTToBurn
          else
            if burnQuantity == 0
              then propertyViolation v_d_1_0_0_No_ENNOP_To_Burn
              else
                if burnQuantity > 0
                  then propertyViolation v_d_1_0_1_No_Minting_Allowed
                  else propertyViolation v_d_1_0_2_More_Than_One_ENOP_To_Burn
      _ -> propertyViolation v_d_1_1_1_ENOP_NFT_Cardinality_Above_1
  )
    . valueOf' (txInfoMint . scriptContextTxInfo $ ctx)

{-# INLINEABLE valueOf' #-}
valueOf' :: Value -> CurrencySymbol -> [(TokenName, Integer)]
valueOf' (Value mp) cur =
  maybe [] Map.toList (Map.lookup cur mp)
