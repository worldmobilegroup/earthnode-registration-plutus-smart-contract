{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Deregister.TxBuilding (
  deregister,
  deregisterWithoutProvidingENOPNFT,
  deregisterWithMintingENOPNFT,
  deregisterByBurningENOPWithoutReleasingENNFT,
  deregisterWithENOPTokenNameDifferentThanENNFTokenName,
  deregisterWithMultipleENOPNFTs,
  deregisterWithENNFTCardinalityAbove1,
  deregisterWihtoutRegistrationValidatorInput,
  deregisterWith2WalletSigning,
) where

import Cooked

import Data.Default (Default (def))

import Data.Map qualified as Map
import PlutusTx.AssocMap qualified as PMap

import Data.Set qualified as Set

import Adapter.CardanoCryptoClass.Crypto
import Aya.Registration.Core.ENOPNFT.MonetaryPolicy.Builder (Action (..))
import Aya.Registration.Core.Validator.Builder
import Aya.Registration.Core.Validator.OnChain

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import PlutusLedgerApi.V3 (
  Value (..),
  fromBuiltin,
  toBuiltin,
 )
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Maybe (isJust)
import Safe (headMay)
import Specs.Aya.Registration.Core.Model (Commission, ENOPNFT, NFT (..))

deregister
  :: (MonadBlockChain m)
  => (RegistrationValidatorSettings, ENOPNFT)
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
deregister registratedItemId@(settings, enopNFT) operator = do
  registeredItemRef <-
    ( fst
        . fromMaybe (error "No ENNFT found on registration validator utxos.")
        . headMay
        <$>
      )
      . runUtxoSearch
      . flip filterWithPred (containsENNFT registratedItemId . outputValue)
      . utxosAtSearch
      $ registrationValidatorAddress settings

  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Burn, tokenName enopNFT, -1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript Deregister)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

deregisterWith2WalletSigning
  :: (MonadBlockChain m)
  => (RegistrationValidatorSettings, ENOPNFT)
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
deregisterWith2WalletSigning registratedItemId@(settings, enopNFT) operator anotherOperator = do
  registeredItemRef <-
    ( fst
        . fromMaybe (error "No ENNFT found on registration validator utxos.")
        . headMay
        <$>
      )
      . runUtxoSearch
      . flip filterWithPred (containsENNFT registratedItemId . outputValue)
      . utxosAtSearch
      $ registrationValidatorAddress settings

  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Burn, tokenName enopNFT, -1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator, anotherOperator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript Deregister)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

deregisterWihtoutRegistrationValidatorInput
  :: (MonadBlockChain m)
  => (RegistrationValidatorSettings, ENOPNFT)
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
deregisterWihtoutRegistrationValidatorInput registratedItemId@(settings, enopNFT) operator = do
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Burn, tokenName enopNFT, -1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            []
        }
  return registratedItemId

deregisterWithoutProvidingENOPNFT
  :: (MonadBlockChain m)
  => (RegistrationValidatorSettings, ENOPNFT)
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
deregisterWithoutProvidingENOPNFT registratedItemId@(settings, enopNFT) operator = do
  registeredItemRef <-
    ( fst
        . fromMaybe (error "No ENNFT found on registration validator utxos.")
        . headMay
        <$>
      )
      . runUtxoSearch
      . flip filterWithPred (containsENNFT registratedItemId . outputValue)
      . utxosAtSearch
      $ registrationValidatorAddress settings

  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty -- no ENOPNFT to Burn
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript Deregister)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

deregisterWithMintingENOPNFT
  :: (MonadBlockChain m)
  => (RegistrationValidatorSettings, ENOPNFT)
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
deregisterWithMintingENOPNFT registratedItemId@(settings, enopNFT) operator = do
  registeredItemRef <-
    ( fst
        . fromMaybe (error "No ENNFT found on registration validator utxos.")
        . headMay
        <$>
      )
      . runUtxoSearch
      . flip filterWithPred (containsENNFT registratedItemId . outputValue)
      . utxosAtSearch
      $ registrationValidatorAddress settings

  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Burn, tokenName enopNFT, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript Deregister)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

deregisterByBurningENOPWithoutReleasingENNFT
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
deregisterByBurningENOPWithoutReleasingENNFT keyPair registratedItemId@(settings, enopNFT) givenCommission operator = do
  registeredItemRef <-
    ( fst
        . fromMaybe (error "No ENNFT found on registration validator utxos.")
        . headMay
        <$>
      )
      . runUtxoSearch
      . flip filterWithPred (containsENNFT registratedItemId . outputValue)
      . utxosAtSearch
      $ registrationValidatorAddress settings
  let signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName enopNFT)
                (walletPKHash operator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Burn, tokenName enopNFT, -1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript Deregister)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash operator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

deregisterWithENOPTokenNameDifferentThanENNFTokenName
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> ENOPNFT
  -> Commission
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
deregisterWithENOPTokenNameDifferentThanENNFTokenName keyPair registratedItemId@(settings, enopNFT) enopNFT2 givenCommission operator = do
  registeredItemRef <-
    ( fst
        . fromMaybe (error "No ENNFT found on registration validator utxos.")
        . headMay
        <$>
      )
      . runUtxoSearch
      . flip filterWithPred (containsENNFT registratedItemId . outputValue)
      . utxosAtSearch
      $ registrationValidatorAddress settings
  let signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName enopNFT)
                (walletPKHash operator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Burn, tokenName enopNFT2, -1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript Deregister)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash operator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

deregisterWithMultipleENOPNFTs
  :: (MonadBlockChain m)
  => (RegistrationValidatorSettings, ENOPNFT)
  -> ENOPNFT
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
deregisterWithMultipleENOPNFTs registratedItemId@(settings, enopNFT) enopNFT2 operator = do
  registeredItemRef <-
    ( fst
        . fromMaybe (error "No ENNFT found on registration validator utxos.")
        . headMay
        <$>
      )
      . runUtxoSearch
      . flip filterWithPred (containsENNFT registratedItemId . outputValue)
      . utxosAtSearch
      $ registrationValidatorAddress settings

  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList
              [ (associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Burn, tokenName enopNFT, -1)
              , (associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Burn, tokenName enopNFT2, -1)
              ]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript Deregister)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

deregisterWithENNFTCardinalityAbove1
  :: (MonadBlockChain m)
  => (RegistrationValidatorSettings, ENOPNFT)
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
deregisterWithENNFTCardinalityAbove1 registratedItemId1@(settings, enopNFT1) registratedItemId2@(_, enopNFT2) operator = do
  registeredItemRef1 <-
    ( fst
        . fromMaybe (error "No ENNFT found on registration validator utxos.")
        . headMay
        <$>
      )
      . runUtxoSearch
      . flip filterWithPred (containsENNFT registratedItemId1 . outputValue)
      . utxosAtSearch
      $ registrationValidatorAddress settings
  registeredItemRef2 <-
    ( fst
        . fromMaybe (error "No ENNFT found on registration validator utxos.")
        . headMay
        <$>
      )
      . runUtxoSearch
      . flip filterWithPred (containsENNFT registratedItemId2 . outputValue)
      . utxosAtSearch
      $ registrationValidatorAddress settings
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Burn, tokenName enopNFT1, -1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef1 (TxSkelRedeemerForScript Deregister)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT1) 1)
            ]
        }

  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Burn, tokenName enopNFT2, -1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef2 (TxSkelRedeemerForScript Deregister)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT1) 1)
            , paysPK
                operator
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT2) 1)
            ]
        }
  return registratedItemId1

containsENNFT :: (RegistrationValidatorSettings, ENOPNFT) -> Value -> Bool
containsENNFT (settings, enopNFT) (Value v) =
  let currency = ennftCurrencySymbol settings
      tn = tokenName enopNFT
   in isJust (PMap.lookup currency v >>= PMap.lookup tn)
