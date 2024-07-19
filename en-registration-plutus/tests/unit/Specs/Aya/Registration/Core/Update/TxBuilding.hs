{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Update.TxBuilding (
  update,
  updateWithoutENOPNFTASInput,
  updateWithoutENNFTOnRegistrationValidatorOutput,
  updateAndMintSomething,
  updateAndBurnSomething,
  updateWithDifferentENandENOPNFTTokenNames,
  updateMultipleValidatorsDetailsAtOnce,
  updateWithMoreThanOneENNFT,
  updateWithMoreThanOneENOPNFT,
  updateAndGiveTheENOPNFTToAnotherOperator,
  updateWith2WalletSigning,
  updateWithWrongEnnftTokenNameDatum,
  updateWithWrongEnopNftCurrencySymbolInDatum,
  updateWithInvalidDatumVerification,
  updateWihoutValidatorOutput,
  updateWithNoRegistrationDatum,
  updateWithHashedRegistrationDatum,
) where

import Cooked

import Data.Default (Default (def))

import Data.Map qualified as Map
import PlutusTx.AssocMap qualified as PMap

import Data.Set qualified as Set

import Adapter.CardanoCryptoClass.Crypto (
  DSIGNAlgorithm (..),
  KeyPair (signatureKey, verificationKey),
  ToByteString (toByteString),
  sign,
 )
import Aya.Registration.Core.Validator.Builder (
  associatedENOPNFTCurrencySymbol,
  registrationValidatorAddress,
  typedRegistrationValidator,
 )
import Aya.Registration.Core.Validator.OnChain (
  RegistrationAction (..),
  RegistrationDatum (..),
  RegistrationValidatorSettings (..),
  mkHashedRegistrationMessage,
 )
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Ledger.Scripts qualified as Script
import PlutusLedgerApi.V3 (
  CurrencySymbol,
  Value (..),
  fromBuiltin,
  toBuiltin,
 )
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Maybe (isJust)
import Safe (headMay)
import Specs.Aya.Registration.Core.Model (Commission, ENNFT, ENOPNFT, NFT (..))

update
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
update keyPair registratedItemId@(settings, enopNFT) newCommission operator newOperator = do
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
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateWith2WalletSigning
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWith2WalletSigning keyPair registratedItemId@(settings, enopNFT) newCommission operator newOperator = do
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
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator, newOperator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateWithoutENOPNFTASInput
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWithoutENOPNFTASInput keyPair registratedItemId@(settings, enopNFT) newCommission operator newOperator = do
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
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  -- Provide the ENOPNFT to new operator before updating the registration details to trigger the property violation
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                newOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }

  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateWithoutENNFTOnRegistrationValidatorOutput
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWithoutENNFTOnRegistrationValidatorOutput keyPair registratedItemId@(settings, enopNFT) newCommission operator newOperator = do
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
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                ( V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1
                    <> V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1
                )
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (ada 5)
            ]
        }
  return registratedItemId

updateAndMintSomething
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateAndMintSomething keyPair registratedItemId@(settings, enopNFT) newCommission operator newOperator = do
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
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(Script.Versioned quickCurrencyPolicy Script.PlutusV3, NoMintsRedeemer, "banana", 10)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                ( V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1
                    <> V3.singleton quickCurrencySymbol "banana" 10
                )
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateAndBurnSomething
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateAndBurnSomething keyPair registratedItemId@(settings, enopNFT) newCommission operator newOperator = do
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
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = txSkelMintsFromList [(Script.Versioned quickCurrencyPolicy Script.PlutusV3, NoMintsRedeemer, "banana", 5)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton quickCurrencySymbol "banana" 5)
            ]
        }

  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(Script.Versioned quickCurrencyPolicy Script.PlutusV3, NoMintsRedeemer, "banana", -5)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateWithDifferentENandENOPNFTTokenNames
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> ENOPNFT
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWithDifferentENandENOPNFTTokenNames keyPair registratedItemId@(settings, registeredEnopNFT) anotherENOPNFT newCommission operator newOperator = do
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
                (tokenName registeredEnopNFT)
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName anotherENOPNFT) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName registeredEnopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName registeredEnopNFT) 1)
            ]
        }
  return registratedItemId

updateMultipleValidatorsDetailsAtOnce
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateMultipleValidatorsDetailsAtOnce keyPair registratedItemId1@(settings, registeredEnopNFT1) registratedItemId2@(_, registeredEnopNFT2) newCommission operator newOperator = do
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

  let signedMessage1 =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName registeredEnopNFT1)
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
      signedMessage2 =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName registeredEnopNFT2)
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns =
            Map.singleton registeredItemRef1 (TxSkelRedeemerForScript UpdateRegistrationDetails)
              <> Map.singleton registeredItemRef2 (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName registeredEnopNFT1) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage1
                    , ennftTokenName = tokenName registeredEnopNFT1
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName registeredEnopNFT1) 1)
            , paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName registeredEnopNFT2) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage2
                    , ennftTokenName = tokenName registeredEnopNFT1
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName registeredEnopNFT2) 1)
            ]
        }
  return registratedItemId1

updateWithMoreThanOneENOPNFT
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWithMoreThanOneENOPNFT keyPair registratedItemId1@(settings, registeredEnopNFT1) (_, registeredEnopNFT2) newCommission operator newOperator = do
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

  let signedMessage1 =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName registeredEnopNFT1)
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef1 (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName registeredEnopNFT1) 1)
            , paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName registeredEnopNFT2) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage1
                    , ennftTokenName = tokenName registeredEnopNFT1
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName registeredEnopNFT1) 1)
            ]
        }
  return registratedItemId1

updateWithMoreThanOneENNFT
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> ENNFT
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWithMoreThanOneENNFT keyPair registratedItemId1@(settings, registeredEnopNFT1) anotherEnNFT newCommission operator newOperator = do
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

  let signedMessage1 =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName registeredEnopNFT1)
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef1 (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName registeredEnopNFT1) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage1
                    , ennftTokenName = tokenName registeredEnopNFT1
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                ( V3.singleton (ennftCurrencySymbol settings) (tokenName registeredEnopNFT1) 1
                    <> V3.singleton (ennftCurrencySymbol settings) (tokenName anotherEnNFT) 1
                )
            ]
        }
  return registratedItemId1

updateAndGiveTheENOPNFTToAnotherOperator
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateAndGiveTheENOPNFTToAnotherOperator keyPair registratedItemId@(settings, enopNFT) newCommission operator newOperator = do
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
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                newOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateWithWrongEnnftTokenNameDatum
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> ENNFT
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWithWrongEnnftTokenNameDatum keyPair registratedItemId@(settings, enopNFT) wrongEnnft newCommission operator newOperator = do
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
                (tokenName wrongEnnft)
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName wrongEnnft
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateWithWrongEnopNftCurrencySymbolInDatum
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> CurrencySymbol
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWithWrongEnopNftCurrencySymbolInDatum keyPair registratedItemId@(settings, enopNFT) anotherDifferentENNftCurrencySymbol newCommission operator newOperator = do
  let anotherSetting = RegistrationValidatorSettings anotherDifferentENNftCurrencySymbol
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
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol anotherSetting)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol anotherSetting
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateWithInvalidDatumVerification
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> ByteString
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWithInvalidDatumVerification keyPair registratedItemId@(settings, enopNFT) newCommission operator newOperator invalidSignature = do
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
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin invalidSignature
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateWihoutValidatorOutput
  :: (MonadBlockChain m)
  => (RegistrationValidatorSettings, ENOPNFT)
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWihoutValidatorOutput registratedItemId@(settings, enopNFT) operator = do
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
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            , paysPK
                operator
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateWithNoRegistrationDatum
  :: (MonadBlockChain m)
  => (RegistrationValidatorSettings, ENOPNFT)
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWithNoRegistrationDatum registratedItemId@(settings, enopNFT) operator = do
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
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            , paysScriptNoDatum
                (typedRegistrationValidator settings)
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

updateWithHashedRegistrationDatum
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
updateWithHashedRegistrationDatum keyPair registratedItemId@(settings, enopNFT) newCommission operator newOperator = do
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
                (walletPKHash newOperator)
                newCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = Map.empty
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [operator]
        , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                operator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
            , paysScriptDatumHash
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName enopNFT
                    , cardanoRewardPubKey = walletPKHash newOperator
                    , commission = newCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
            ]
        }
  return registratedItemId

containsENNFT :: (RegistrationValidatorSettings, ENOPNFT) -> Value -> Bool
containsENNFT (settings, enopNFT) (Value v) =
  let currency = ennftCurrencySymbol settings
      tn = tokenName enopNFT
   in isJust (PMap.lookup currency v >>= PMap.lookup tn)
