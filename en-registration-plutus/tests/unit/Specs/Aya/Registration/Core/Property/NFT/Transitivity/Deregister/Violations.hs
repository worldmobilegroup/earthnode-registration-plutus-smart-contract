{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.NFT.Transitivity.Deregister.Violations (
  specs,
) where

import Test.Tasty

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)

import Adapter.Cooked
import Aya.Registration.Core.Property.NFT.Transitivity.Deregister
import qualified Data.List.NonEmpty as NL
import Specs.Aya.Registration.Core.Deregister.TxBuilding (
  deregisterByBurningENOPWithoutReleasingENNFT,
  deregisterWithENNFTCardinalityAbove1,
  deregisterWithENOPTokenNameDifferentThanENNFTokenName,
  deregisterWithMintingENOPNFT,
  deregisterWithMultipleENOPNFTs,
  deregisterWithoutProvidingENOPNFT,
 )
import Specs.Aya.Registration.Core.Model
import Specs.Aya.Registration.Core.Register.Fixture
import Specs.Aya.Registration.Core.Register.TxBuilding (
  register,
 )
import Specs.Aya.Registration.Core.Update.TxBuilding
import Test.Tasty.QuickCheck (
  forAll,
  testProperty,
 )

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
  testGroup
    "Deregister"
    [ testGroup
        "Property 1.0 : Tokens Quantities are verified"
        [ testProperty "d.1.0.0 violation - No ENNOP To Burn " $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_d_1_0_0_No_ENNOP_To_Burn
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair ennft commission operator
                    registrationUpdateReference <- update substrateKeyPair registrationReference commission operator anotherOperator
                    deregisterWithoutProvidingENOPNFT registrationUpdateReference operator
        , testProperty "d.1.0.1 violation - No Minting Allowed" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_d_1_0_1_No_Minting_Allowed
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair ennft commission operator
                    registrationUpdateReference <- update substrateKeyPair registrationReference commission operator anotherOperator
                    deregisterWithMintingENOPNFT registrationUpdateReference operator
        , testProperty
            "d.1.0.2 violation - ENOP NFT Quantity to Burn > 1  (enforced by Non Fungible Property of EN Tokens )"
            True
        , testProperty "d.1.0.3 violation - No ENNFT Released To Operator" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_d_1_0_3_No_ENNFT_Released_To_Operator
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair ennft commission operator
                    registrationUpdateReference <- update substrateKeyPair registrationReference commission operator anotherOperator
                    deregisterByBurningENOPWithoutReleasingENNFT substrateKeyPair registrationUpdateReference commission operator
        ]
    , testGroup
        "Property 1.1 : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT"
        [ testProperty "u.1.1.0 violation - ENOPNFT TokenName =/ ENNFT TokenName" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_d_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair (NFT ennftCurrencySymbol firstEnnftTn) commission operator
                    registrationUpdateReference <- update substrateKeyPair registrationReference commission operator anotherOperator
                    (_, enopNFT2) <- register substrateKeyPair (NFT ennftCurrencySymbol (NL.head ennftsTokenNames)) commission operator
                    deregisterWithENOPTokenNameDifferentThanENNFTokenName
                      substrateKeyPair
                      registrationUpdateReference
                      enopNFT2
                      commission
                      operator
        , testProperty "d.1.1.1 violation - |ENOP NFT| > 1 (Cardinality Violation)" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_d_1_1_1_ENOP_NFT_Cardinality_Above_1
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ do
                    registrationReference1 <- register substrateKeyPair (NFT ennftCurrencySymbol firstEnnftTn) commission operator
                    (_, enopNFT2) <- register substrateKeyPair (NFT ennftCurrencySymbol (NL.head ennftsTokenNames)) commission operator
                    deregisterWithMultipleENOPNFTs
                      registrationReference1
                      enopNFT2
                      operator
        , testProperty "d.1.1.2 violation - |EN NFT|   > 1 (Cardinality Violation)" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_d_1_1_2_ENNFT_Cardinality_Above_1
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ do
                    registrationReference1 <- register substrateKeyPair (NFT ennftCurrencySymbol firstEnnftTn) commission operator
                    registrationReference2 <-
                      register substrateKeyPair (NFT ennftCurrencySymbol (NL.head ennftsTokenNames)) commission operator
                    deregisterWithENNFTCardinalityAbove1
                      registrationReference1
                      registrationReference2
                      operator
        ]
    ]
