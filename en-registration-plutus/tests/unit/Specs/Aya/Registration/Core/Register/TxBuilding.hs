{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Register.TxBuilding (
  register,
  -- Pushing Fungible Tokens instead of NFTs
  registerMintingAnENOPWithQuantityAbove1,
  registerWithanENNFTWithAQuantityAbove1,
  -- Varying the Quantity of NFTs
  registerWithoutAnENNFT,
  registerWithDifferentENandENOPNFTTokenNames,
  registerByGeneratingMoreThan1ENOPNFT,
  registerByGeneratingMoreThan1ENNFT,
  -- Varying the Signers
  registerAndMintToAnotherOperator,
  registerWith2WalletsSigning,
  registerWithoutSigning,
  -- Varying the datum
  registerWithInvalidDatumVerification,
  mintWithoutRegistrationScript,
  registerWithNoRegistrationDatum,
  registerWithHashedRegistrationDatum,
  registerMoreThanOneOperator,
  registerWithWrongEnnftTokenNameDatum,
  registerWithWrongEnopNftCurrencySymbolInDatum,
) where

import Cooked

import Data.Default (Default (def))

import Data.Map qualified as Map

import Aya.Registration.Core.ENOPNFT.MonetaryPolicy.OnChain (Action (..))
import Data.Set qualified as Set

import Adapter.CardanoCryptoClass.Crypto
import Data.ByteString (ByteString)

import Aya.Registration.Core.Validator.Builder (
  associatedENOPNFTCurrencySymbol,
  associatedENOPNFTMonetaryPolicy,
  typedRegistrationValidator,
 )
import Aya.Registration.Core.Validator.OnChain (
  RegistrationDatum (..),
  RegistrationValidatorSettings (..),
  mkHashedRegistrationMessage,
 )
import Data.List.NonEmpty qualified as NL
import PlutusLedgerApi.V3 (CurrencySymbol, TokenName, fromBuiltin, toBuiltin)
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import Specs.Aya.Registration.Core.Model

register
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Commission
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
register keyPair ennft givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }

  return (settings, NFT (associatedENOPNFTCurrencySymbol settings) (tokenName ennft))

registerMintingAnENOPWithQuantityAbove1
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Commission
  -> Wallet
  -> m ()
registerMintingAnENOPWithQuantityAbove1 keyPair ennft givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 2)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 2)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }
  return ()

registerWithoutAnENNFT
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Commission
  -> Wallet
  -> m ENOPNFT
registerWithoutAnENNFT keyPair ennft givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (ada 3) -- withtout the ENNFT
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      (tokenName ennft)

registerWithanENNFTWithAQuantityAbove1
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Quantity
  -> Commission
  -> Wallet
  -> m ENOPNFT
registerWithanENNFTWithAQuantityAbove1 keyPair ennft ennftQuantityAbove1 givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) ennftQuantityAbove1)
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      (tokenName ennft)

registerByGeneratingMoreThan1ENNFT
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (CurrencySymbol, TokenName, NL.NonEmpty TokenName)
  -> Commission
  -> Wallet
  -> m ENOPNFT
registerByGeneratingMoreThan1ENNFT keyPair (givenEnnftCurrencySymbol, firstEnnftTn, ennftTns) givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings givenEnnftCurrencySymbol
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                firstEnnftTn
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints = txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, firstEnnftTn, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) firstEnnftTn 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = firstEnnftTn
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (foldMap (\tn -> V3.singleton givenEnnftCurrencySymbol tn 1) (ennftTns <> NL.fromList [firstEnnftTn]))
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      firstEnnftTn

registerByGeneratingMoreThan1ENOPNFT
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> NL.NonEmpty TokenName
  -> Commission
  -> Wallet
  -> m [ENOPNFT]
registerByGeneratingMoreThan1ENOPNFT keyPair ennft differentTokenNames givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList $
              (associatedENOPNFTMonetaryPolicy settings,SomeMintsRedeemer Mint,,1)
                <$> (tokenName ennft : NL.toList differentTokenNames)
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                ( nfts
                    (associatedENOPNFTCurrencySymbol settings)
                    (tokenName ennft : NL.toList differentTokenNames)
                )
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }
  return $
    NFT (associatedENOPNFTCurrencySymbol settings)
      <$> (tokenName ennft : NL.toList differentTokenNames)

registerAndMintToAnotherOperator
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Commission
  -> Wallet
  -> Wallet
  -> m ENOPNFT
registerAndMintToAnotherOperator keyPair ennft givenCommission givenOperator anotherGivenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                anotherGivenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      (tokenName ennft)

registerWith2WalletsSigning
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Commission
  -> Wallet
  -> Wallet
  -> m ENOPNFT
registerWith2WalletsSigning keyPair ennft givenCommission givenOperator anotherWallet = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator, anotherWallet]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      (tokenName ennft)

registerWithoutSigning
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Commission
  -> Wallet
  -> m ENOPNFT
registerWithoutSigning keyPair ennft givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = []
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      (tokenName ennft)

registerWithDifferentENandENOPNFTTokenNames
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> TokenName
  -> Commission
  -> Wallet
  -> m ENOPNFT
registerWithDifferentENandENOPNFTTokenNames keyPair ennft enoptTokenName givenCommission givenWallet = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenWallet)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, enoptTokenName, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenWallet]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenWallet
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) enoptTokenName 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenWallet
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      (tokenName ennft)

registerWithWrongEnopNftCurrencySymbolInDatum
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> CurrencySymbol
  -> Commission
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
registerWithWrongEnopNftCurrencySymbolInDatum keyPair ennft anotherDifferentENNftCurrencySymbol givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      anotherSetting = RegistrationValidatorSettings anotherDifferentENNftCurrencySymbol
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol anotherSetting) -- using a different currency symbol in datum for ENOPNFT
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol anotherSetting
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }

  return (settings, NFT (associatedENOPNFTCurrencySymbol settings) (tokenName ennft))

registerWithWrongEnnftTokenNameDatum
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> ENNFT
  -> Commission
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
registerWithWrongEnnftTokenNameDatum keyPair ennft wrongEnnft givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName wrongEnnft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName wrongEnnft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }

  return (settings, NFT (associatedENOPNFTCurrencySymbol settings) (tokenName ennft))

registerWithInvalidDatumVerification
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Commission
  -> Wallet
  -> ByteString
  -> m ENOPNFT
registerWithInvalidDatumVerification keyPair ennft givenCommission givenOperator invalidSignature = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin invalidSignature
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      (tokenName ennft)

registerWithNoRegistrationDatum
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Commission
  -> Wallet
  -> m ENOPNFT
registerWithNoRegistrationDatum _ ennft _ givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft

  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptNoDatum
                (typedRegistrationValidator settings)
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      (tokenName ennft)

registerWithHashedRegistrationDatum
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Commission
  -> Wallet
  -> m ENOPNFT
registerWithHashedRegistrationDatum keyPair ennft givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
      signedMessage =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName ennft)
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysScriptDatumHash
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage
                    , ennftTokenName = tokenName ennft
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      (tokenName ennft)

mintWithoutRegistrationScript
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> ENNFT
  -> Wallet
  -> m ENOPNFT
mintWithoutRegistrationScript _ ennft givenOperator = do
  let settings = RegistrationValidatorSettings . currencySymbol $ ennft
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
            , paysPK
                givenOperator
                (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
            ]
        }
  return $
    NFT
      (associatedENOPNFTCurrencySymbol settings)
      (tokenName ennft)

registerMoreThanOneOperator
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (CurrencySymbol, TokenName, TokenName)
  -> Commission
  -> Wallet
  -> m (ENOPNFT, ENOPNFT)
registerMoreThanOneOperator keyPair (currencySymbolEnnfts, ennft1Tn, ennft2Tn) givenCommission givenOperator = do
  let settings = RegistrationValidatorSettings currencySymbolEnnfts
      signedMessage1 =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                ennft1Tn
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
      signedMessage2 =
        sign
          (signatureKey keyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                ennft2Tn
                (walletPKHash givenOperator)
                givenCommission
                (associatedENOPNFTCurrencySymbol settings)
          )
  _ <-
    validateTxSkel $
      txSkelTemplate
        { txSkelMints =
            txSkelMintsFromList
              [ (associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, ennft1Tn, 1)
              , (associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, ennft2Tn, 1)
              ]
        , txSkelLabel = Set.empty
        , txSkelOpts = def{txOptEnsureMinAda = True}
        , txSkelValidityRange = Api.always
        , txSkelSigners = [givenOperator]
        , txSkelIns = Map.empty
        , txSkelInsReference = Set.empty
        , txSkelOuts =
            [ paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) ennft1Tn 1)
            , paysPK
                givenOperator
                (V3.singleton (associatedENOPNFTCurrencySymbol settings) ennft2Tn 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage1
                    , ennftTokenName = ennft1Tn
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton currencySymbolEnnfts ennft1Tn 1)
            , paysScriptInlineDatum
                (typedRegistrationValidator settings)
                ( RegistrationDatum
                    { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                    , signature = toBuiltin signedMessage2
                    , ennftTokenName = ennft2Tn
                    , cardanoRewardPubKey = walletPKHash givenOperator
                    , commission = givenCommission
                    , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                    }
                )
                (V3.singleton currencySymbolEnnfts ennft2Tn 1)
            ]
        }
  return
    ( NFT
        (associatedENOPNFTCurrencySymbol settings)
        ennft1Tn
    , NFT
        (associatedENOPNFTCurrencySymbol settings)
        ennft2Tn
    )
