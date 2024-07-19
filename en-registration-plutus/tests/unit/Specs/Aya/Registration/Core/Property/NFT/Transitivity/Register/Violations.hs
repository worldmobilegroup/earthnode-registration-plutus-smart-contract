{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.NFT.Transitivity.Register.Violations (
  specs,
) where

import Test.Tasty

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)

import Adapter.Cooked
import Aya.Registration.Core.Property.NFT.Transitivity.Register
import qualified Data.List.NonEmpty as NL
import Specs.Aya.Registration.Core.Model
import Specs.Aya.Registration.Core.Register.Fixture
import Specs.Aya.Registration.Core.Register.TxBuilding (
  registerByGeneratingMoreThan1ENNFT,
  registerByGeneratingMoreThan1ENOPNFT,
  registerMintingAnENOPWithQuantityAbove1,
  registerWithDifferentENandENOPNFTTokenNames,
  registerWithanENNFTWithAQuantityAbove1,
  registerWithoutAnENNFT,
 )
import Test.Tasty.QuickCheck (
  forAll,
  testProperty,
 )

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
  testGroup
    "Register"
    [ testGroup
        "Property 1.0 : Tokens Quantities are verified"
        [ testProperty
            "r.1.0.0 violation - No ENNOP Minted (can't be enforced)"
            True
        , testProperty "r.1.0.1 violation - ENNOP Minted Quantity > 1" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_r_1_0_1_ENNOP_Minted_Quantity_Above_One
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ registerMintingAnENOPWithQuantityAbove1
                    substrateKeyPair
                    (NFT ennftCurrencySymbol firstEnnftTn)
                    commission
                    operator
        , testProperty "r.1.0.2 violation - No ENNFT on Registration validator output" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_r_1_0_2_No_ENNFT_On_Validator_Output
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ registerWithoutAnENNFT
                    substrateKeyPair
                    ennft
                    commission
                    operator
        , testProperty "r.1.0.3 violation - ENNFT Quantity > 1" $
            forAll (genFixtureENNFTWithWrongQuantityAbove1 keys) $
              \FixtureENNFTWithInvalidQuantityAbove1{..} ->
                shouldViolateAProperty
                  v_r_1_0_3_ENNFT_Quantity_Above_One
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ registerWithanENNFTWithAQuantityAbove1
                    substrateKeyPair
                    ennft
                    wrongENNFTQuantity
                    commission
                    operator
        ]
    , testGroup
        "Property 1.1 : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT"
        [ testProperty "r.1.1.0 violation - ENOPNFT TokenName =/ ENNFT TokenName" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_r_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ registerWithDifferentENandENOPNFTTokenNames
                    substrateKeyPair
                    (NFT ennftCurrencySymbol firstEnnftTn)
                    (NL.head ennftsTokenNames)
                    commission
                    operator
        , testProperty "r.1.1.1 violation - |ENOP NFT| > 1 (Cardinality Violation)" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_r_1_1_1_ENOP_NFT_Cardinality_Above_1
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ registerByGeneratingMoreThan1ENOPNFT
                    substrateKeyPair
                    (NFT ennftCurrencySymbol firstEnnftTn)
                    ennftsTokenNames
                    commission
                    operator
        , testProperty "r.1.1.2 violation - |EN NFT|   > 1 (Cardinality Violation)" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_r_1_1_2_ENNFT_Cardinality_Above_1
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ registerByGeneratingMoreThan1ENNFT
                    substrateKeyPair
                    (ennftCurrencySymbol, firstEnnftTn, ennftsTokenNames)
                    commission
                    operator
        ]
    ]
