{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.NFT.Transitivity.Update.Violations (
  specs,
) where

import Test.Tasty

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)

import Adapter.Cooked
import Aya.Registration.Core.Property.NFT.Transitivity.Update
import qualified Data.List.NonEmpty as NL
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
    "Update"
    [ testGroup
        "Property 1.0 : Tokens Quantities are verified"
        [ testProperty "u.1.0.0 violation - No ENNOP as Input" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_u_1_0_0_ENNOP_Not_Output_to_Operator
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair ennft commission operator
                    updateWithoutENOPNFTASInput substrateKeyPair registrationReference commission operator anotherOperator
        , testProperty "u.1.0.1 violation - No ENNFT on Registration validator output" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_u_1_0_1_No_ENNFT_On_Validator_Output
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair ennft commission operator
                    updateWithoutENNFTOnRegistrationValidatorOutput
                      substrateKeyPair
                      registrationReference
                      commission
                      operator
                      anotherOperator
        , testProperty "u.1.0.2 violation - No Minting Allowed" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_u_1_0_2_No_Minting_Allowed
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair ennft commission operator
                    updateAndMintSomething substrateKeyPair registrationReference commission operator anotherOperator
        , testProperty "u.1.0.3 violation - No Burning Allowed" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_u_1_0_3_No_Burning_Allowed
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair ennft commission operator
                    updateAndBurnSomething substrateKeyPair registrationReference commission operator anotherOperator
        ]
    , testGroup
        "Property 1.1 : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT"
        [ testProperty "u.1.1.0 violation - ENOPNFT TokenName =/ ENNFT TokenName" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_u_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair (NFT ennftCurrencySymbol firstEnnftTn) commission operator
                    (_, enopNFT2) <- register substrateKeyPair (NFT ennftCurrencySymbol (NL.head ennftsTokenNames)) commission operator
                    updateWithDifferentENandENOPNFTTokenNames
                      substrateKeyPair
                      registrationReference
                      enopNFT2
                      commission
                      operator
                      anotherOperator
        , testProperty "u.1.1.1 violation - |ENOP NFT| > 1 (Cardinality Violation)" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_u_1_1_1_ENOP_NFT_Cardinality_Above_1
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ do
                    registrationReference1 <- register substrateKeyPair (NFT ennftCurrencySymbol firstEnnftTn) commission operator
                    registrationReference2 <-
                      register substrateKeyPair (NFT ennftCurrencySymbol (NL.head ennftsTokenNames)) commission operator
                    updateWithMoreThanOneENOPNFT
                      substrateKeyPair
                      registrationReference1
                      registrationReference2
                      commission
                      operator
                      anotherOperator
        , testProperty "u.1.1.2 violation - |EN NFT|   > 1 (Cardinality Violation)" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_u_1_1_2_ENNFT_Cardinality_Above_1
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ do
                    registrationReference1 <- register substrateKeyPair (NFT ennftCurrencySymbol firstEnnftTn) commission operator
                    updateWithMoreThanOneENNFT
                      substrateKeyPair
                      registrationReference1
                      (NFT ennftCurrencySymbol (NL.head ennftsTokenNames))
                      commission
                      operator
                      anotherOperator
        ]
    ]
