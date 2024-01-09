{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v2.0
-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# options_ghc -fno-specialise         #-}

module ENOP.MpOffChain
  where
import           Cardano.Api.Shelley            (PlutusScript (..),
                                                 PlutusScriptV2)
import           Codec.Serialise
import qualified Data.ByteString.Lazy           as LB
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.Script.Utils.V2.Scripts as Utils
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude
import           ENOP.Types                 
import           ENOP.MintingPolicy
import qualified Plutus.V1.Ledger.Value  as Value

mp :: BuiltinData -> PlutusV2.MintingPolicy
mp sp = PlutusV2.mkMintingPolicyScript
       ($$(PlutusTx.compile [|| vUt ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sp)

script :: BuiltinData -> PlutusV2.Script
script = PlutusV2.getMintingPolicy . mp

scriptHash :: ScriptParams -> PlutusV2.MintingPolicyHash
scriptHash sp = Utils.mintingPolicyHash $ mp (PlutusTx.toBuiltinData sp)

mustMintPolicyCurrencySymbol :: ScriptParams -> Value.CurrencySymbol
mustMintPolicyCurrencySymbol = Value.mpsSymbol . scriptHash

scriptAsCbor :: BuiltinData -> LB.ByteString
scriptAsCbor = serialise . script

mPapiScript :: ScriptParams -> PlutusScript PlutusScriptV2
mPapiScript sp = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor (PlutusTx.toBuiltinData sp)