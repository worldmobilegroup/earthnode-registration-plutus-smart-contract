{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.Datum.Update.Violations (
  specs,
) where

import Test.Tasty

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NL

import Adapter.Cooked (shouldViolateAProperty)
import Aya.Registration.Core.Property.Datum.Register
import Specs.Aya.Registration.Core.Model
import Specs.Aya.Registration.Core.Register.Fixture
import Specs.Aya.Registration.Core.Register.TxBuilding (
  register,
 )
import Specs.Aya.Registration.Core.Update.TxBuilding (
  updateWihoutValidatorOutput,
  updateWithHashedRegistrationDatum,
  updateWithInvalidDatumVerification,
  updateWithNoRegistrationDatum,
  updateWithWrongEnnftTokenNameDatum,
  updateWithWrongEnopNftCurrencySymbolInDatum,
 )
import Test.Tasty.QuickCheck (
  forAll,
  testProperty,
 )

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
  testGroup
    "Update"
    [ testGroup
        "3.0 violation - Registration datum Output should be valid"
        [ testProperty "3.0.1.o violation - `ennftTokenName` field should be equal to the spent ennft token Name" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_3_0_1_o_Registration_Datum_Field_ENNFT_TokenName_Not_Valid
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair (NFT ennftCurrencySymbol firstEnnftTn) commission operator
                    updateWithWrongEnnftTokenNameDatum
                      substrateKeyPair
                      registrationReference
                      (NFT ennftCurrencySymbol (NL.head ennftsTokenNames))
                      commission
                      operator
                      anotherOperator
        , testProperty "3.0.2.o violation - `ennopNftCurrencySymbol` field should be equal to the spent ennop nft currency symbol" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_3_0_2_o_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair (NFT ennftCurrencySymbol firstEnnftTn) commission operator
                    updateWithWrongEnopNftCurrencySymbolInDatum
                      substrateKeyPair
                      registrationReference
                      anotherDifferentENNftCurrencySymbol
                      commission
                      operator
                      anotherOperator
        ]
    , testProperty "3.2.0 violation - Registration datum Output is not Auhtentic" $
        forAll (genFixtureWithInvalidSignature keys) $
          \FixtureWithInvalidSignature{..} ->
            shouldViolateAProperty
              v_3_2_o_Registration_Validator_Output_Datum_Not_Authentic
              (registrationCookedConfig . currencySymbol $ ennft)
              genesis
              $ do
                registrationReference <- register substrateKeyPair ennft commission operator
                updateWithInvalidDatumVerification
                  substrateKeyPair
                  registrationReference
                  commission
                  operator
                  anotherOperator
                  invalidSignature
    , testProperty "3.3.0 violation - Registration validator Output not found (not verifiable)" $
        forAll (genFixtureNominalCase keys) $
          \FixtureNominalCase{..} ->
            shouldViolateAProperty
              v_3_3_o_Registration_Validator_Output_NotFound
              (registrationCookedConfig . currencySymbol $ ennft)
              genesis
              $ do
                registrationReference <- register substrateKeyPair ennft commission operator
                updateWihoutValidatorOutput registrationReference operator
    , testProperty "3.4.0 violation - Registration validator Output has no datum (not verifiable) " $
        forAll (genFixtureNominalCase keys) $
          \FixtureNominalCase{..} ->
            shouldViolateAProperty
              v_3_4_o_Registration_Validator_Output_Has_No_Datum
              (registrationCookedConfig . currencySymbol $ ennft)
              genesis
              $ do
                registrationReference <- register substrateKeyPair ennft commission operator
                updateWithNoRegistrationDatum registrationReference operator
    , testProperty "3.5.0 violation - Registration validator Output has only the hashed datum (not verifiable)" $
        forAll (genFixtureNominalCase keys) $
          \FixtureNominalCase{..} ->
            shouldViolateAProperty
              v_3_5_o_Registration_Validator_Output_Has_Only_Hashed_Datum
              (registrationCookedConfig . currencySymbol $ ennft)
              genesis
              $ do
                registrationReference <- register substrateKeyPair ennft commission operator
                updateWithHashedRegistrationDatum substrateKeyPair registrationReference commission operator anotherOperator
    ]
