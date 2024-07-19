{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Aya.Registration.Core.Validator.Builder (
    typedRegistrationValidator,
    associatedENOPNFTMonetaryPolicySettings,
    associatedENOPNFTMonetaryPolicy,
    associatedENOPNFTCurrencySymbol,
    registrationValidatorAddress,
    mkRegistrationScriptSerialisedScript,
    mkRegistrationScriptScriptCBOREncoded,
) where

import Aya.Registration.Core.ENOPNFT.MonetaryPolicy.Builder qualified as ENNOPNFT (currencySymbol, mkMonetaryPolicy)
import Aya.Registration.Core.ENOPNFT.MonetaryPolicy.OnChain (MonetaryPolicySettings (..))
import Data.Either (Either (Left, Right))
import Data.Function (flip, ($), (.))
import Data.List ((++))

import Aya.Registration.Core.Validator.OnChain (
    RegistrationAction,
    RegistrationDatum,
    RegistrationValidatorSettings (..),
    mkUntypedValidatorFunction,
    mkValidatorFunction,
 )
import Prelude (Show (..), error)

import Adapter.Plutus.OffChain (validatorToTypedValidator)
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Plutus.Script.Utils.Scripts qualified as Script (mkValidatorScript)
import Plutus.Script.Utils.Typed qualified as Script hiding (validatorHash)
import Plutus.Script.Utils.V3.Scripts qualified as Script
import PlutusLedgerApi.V3 (
    ScriptHash (..),
    SerialisedScript,
    serialiseCompiledCode,
 )
import PlutusLedgerApi.V3 qualified as Script
import PlutusTx (
    BuiltinData,
    CompiledCode,
    applyCode,
    compile,
    liftCodeDef,
    unsafeApplyCode,
 )

import Plutus.Script.Utils.Scripts (ValidatorHash (getValidatorHash))

data RegistrationScript

instance Script.ValidatorTypes RegistrationScript where
    type RedeemerType RegistrationScript = RegistrationAction
    type DatumType RegistrationScript = RegistrationDatum

mkVersionedValidator :: RegistrationValidatorSettings -> Script.Versioned Script.Validator
mkVersionedValidator = flip Script.Versioned Script.PlutusV3 . mkRegistrationValidator

mkRegistrationValidator :: RegistrationValidatorSettings -> Script.Validator
mkRegistrationValidator settings =
    case $$(PlutusTx.compile [||\s -> Script.mkUntypedValidator (mkValidatorFunction s)||]) `PlutusTx.applyCode` PlutusTx.liftCodeDef settings of
        Left s -> error $ "Can't apply parameters in validator: " ++ show s
        Right code -> Script.mkValidatorScript code

typedRegistrationValidator :: RegistrationValidatorSettings -> Script.TypedValidator RegistrationScript
typedRegistrationValidator = validatorToTypedValidator . mkRegistrationValidator

getRegistrationValidatorHash :: RegistrationValidatorSettings -> Script.ScriptHash
getRegistrationValidatorHash settings = ScriptHash . getValidatorHash $ Script.validatorHash (mkRegistrationValidator settings)

registrationValidatorAddress :: RegistrationValidatorSettings -> Script.Address
registrationValidatorAddress settings = Script.validatorAddress (typedRegistrationValidator settings)

associatedENOPNFTMonetaryPolicySettings :: RegistrationValidatorSettings -> MonetaryPolicySettings
associatedENOPNFTMonetaryPolicySettings settings@RegistrationValidatorSettings{..} =
    MonetaryPolicySettings
        { ennftCurrencySymbol = ennftCurrencySymbol
        , registrationValidatorHash = getRegistrationValidatorHash settings
        }

associatedENOPNFTMonetaryPolicy :: RegistrationValidatorSettings -> Script.Versioned Script.MintingPolicy
associatedENOPNFTMonetaryPolicy = ENNOPNFT.mkMonetaryPolicy . associatedENOPNFTMonetaryPolicySettings

associatedENOPNFTCurrencySymbol :: RegistrationValidatorSettings -> Script.CurrencySymbol
associatedENOPNFTCurrencySymbol settings = ENNOPNFT.currencySymbol (associatedENOPNFTMonetaryPolicySettings settings)

appliedRegistrationScript ::
    MonetaryPolicySettings ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
appliedRegistrationScript MonetaryPolicySettings{..} =
    $$(compile [||mkUntypedValidatorFunction||])
        `unsafeApplyCode` liftCodeDef (RegistrationValidatorSettings ennftCurrencySymbol)

mkRegistrationScriptSerialisedScript :: MonetaryPolicySettings -> SerialisedScript
mkRegistrationScriptSerialisedScript = serialiseCompiledCode . appliedRegistrationScript

mkRegistrationScriptScriptCBOREncoded :: MonetaryPolicySettings -> ByteString
mkRegistrationScriptScriptCBOREncoded = fromShort . mkRegistrationScriptSerialisedScript
