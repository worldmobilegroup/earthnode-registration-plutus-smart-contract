{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v2.0
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
 
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}

module ENOP.MintingPolicy
  ( vUt
  ) where
import qualified Plutus.V1.Ledger.Value    as Value (valueOf,flattenValue)
import           Plutus.V2.Ledger.Contexts as V2 ()
import           PlutusTx.Prelude           
import           Plutus.V2.Ledger.Api             
import           Plutus.V2.Ledger.Contexts
import           Types                     hiding (ScriptParams,Action)
import           ENOP.Types
import qualified PlutusTx.Builtins        as BI



{-- validateUnregister --}
-- Checks if the ENOP-NFT is burnt.
-- We check that the CurrencySymbol of the token is the one of this minting policy
{-# INLINABLE validateUnregister #-}
validateUnregister :: CurrencySymbol -> TxInfo -> Bool
validateUnregister ocs info
    | traceIfFalse "enOpNftBurnt" $ enOpBurnt ocs (txInfoMint info) info = True
    | otherwise = False

{-- enOpBurnt --}
-- Make sure the Token is burnt
{-# INLINABLE enOpBurnt #-}
enOpBurnt :: CurrencySymbol -> Value ->  TxInfo -> Bool
enOpBurnt cs v info =
  let 
    tn = getTokenName (txInfoInputs info) cs
    burn_amt = case tn of 
        Just tn' -> Value.valueOf v cs tn'
        Nothing -> 0
  in 
    burn_amt == -1 


{-- mintEnOpNft --}
-- Validates that the enopnft is minted only if the registration conditions are met
-- Makes sure the ennft is paid to the registration smart contract and the UTxO contains 
-- a valid ENRegistration Datum with that ennft stored in the datum and
-- the enopnft is named same as the ennft. Also the own currency symbol must be stored correctly in the datum. 
-- Additionally it checks the Aya Signature of this Datum. 
{-# INLINABLE mintEnOpNft #-}
mintEnOpNft :: ScriptParams -> CurrencySymbol -> TxInfo -> Bool
mintEnOpNft ScriptParams{..} ocs info
    |   traceIfFalse "enNftNotSpent" $ Value.valueOf v spNftCs tn == 1 
      , traceIfFalse "enOpMintWrongToken" $ Value.valueOf (txInfoMint info) ocs tn == 1
      , traceIfFalse "checkDatumAndSig" $ checkEnRegDatum $ unsafeFromBuiltinData d = True
    | otherwise = traceIfFalse "otherwise" False
        where 
            -- only one script output to the registration smart contract is allowed
            so = scriptOutputsAt spRegContr info    
            (d,v) = case so of
              [] -> traceError "could not find registration validator outputs"
              [(OutputDatum (Datum od),va)] -> trace "va" (od,va)
              _ -> traceError "more than one registration validator output is not allowed"

            -- the ennft must be spent in this transaction to appear in the correct output at our smart contract so we can lookup the tokenname
            tn = case getTokenName (txInfoInputs info) spNftCs of
                Just tn' -> tn'
                Nothing -> traceError "NoENNFTPresent"
            -- We check if the script output has a correctly formated datum we also verify the datum was signed by the Consensuspubkey
            -- and that the tokenname's of the ennft and  enopnft are matching and the ENOPNFT policy ID is stored correctly in the registration datum.
            checkEnRegDatum :: EnRegistration -> Bool
            checkEnRegDatum dat@EnRegistration {..} = pEnOpCs == ocs && enUsedNftTn == tn && checkDatumSig dat

{-# INLINABLE checkDatumSig #-}
-- Check the signature of the registration datum
checkDatumSig :: EnRegistration -> Bool
checkDatumSig dat@EnRegistration {..} = BI.verifySchnorrSecp256k1Signature enConsensusPubKey (makeMessage dat) enSignature
  
{-# INLINABLE makeMessage #-}
-- Make a signature message EnRegistrationDatum
makeMessage :: EnRegistration -> BuiltinByteString
makeMessage EnRegistration {..} = BI.blake2b_256 $ appendByteString (unCurrencySymbol pEnOpCs) $ consByteString enCommission $ appendByteString (getPubKeyHash enRwdWallet) $ appendByteString (unTokenName enUsedNftTn) $ appendByteString enCceAddress $ appendByteString enMerkleTreeRoot $ appendByteString enOperatorAddress enConsensusPubKey 

{-- getTokenName --}
-- We determine the TokenName from the input of the registration smart contract, 
-- we know it must be exactly one available which must have the same tokenname as the ENOOPNFT
{-# INLINABLE getTokenName #-}
getTokenName :: [TxInInfo] -> CurrencySymbol -> Maybe TokenName
getTokenName is cs = 
  let 
    filter' ::  TxInInfo -> [Maybe TokenName]
    filter' i = fn $ Value.flattenValue (txOutValue $ txInInfoResolved i)
        where 
            fn [] = []
            fn ((cs',tn',amt'):ls) = if cs==cs' && amt' == 1 then [Just tn'] else fn ls
            
    os [] = []
    os (x:xs) = filter' x ++ os xs
  in
    case os is of
      [h] -> h
      _ -> traceError "more than one registration input or none"

-- Main Mintin Policy
{-# INLINABLE mkVal #-}
mkVal :: ScriptParams -> Action -> ScriptContext -> Bool
mkVal sp Mint ctx = mintEnOpNft sp (ownCurrencySymbol ctx) (scriptContextTxInfo ctx)
mkVal _ Burn ctx = validateUnregister (ownCurrencySymbol ctx) (scriptContextTxInfo ctx)

{-# INLINABLE vUt #-}
vUt :: BuiltinData -> BuiltinData -> BuiltinData -> ()
vUt s r c =
   wVal mkVal (unsafeFromBuiltinData s) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

{-# INLINABLE wVal #-}
wVal :: forall s r c
    . (UnsafeFromData s, UnsafeFromData r, UnsafeFromData c)
    => (s -> r -> c -> Bool)
    -> BuiltinData
    -> BuiltinData
    -> BuiltinData
    -> ()
wVal f s r c = check (f (unsafeFromBuiltinData s) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c))