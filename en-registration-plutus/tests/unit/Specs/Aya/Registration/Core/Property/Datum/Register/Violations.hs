{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.Datum.Register.Violations (
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
  mintWithoutRegistrationScript,
  registerMoreThanOneOperator,
  registerWithHashedRegistrationDatum,
  registerWithInvalidDatumVerification,
  registerWithNoRegistrationDatum,
  registerWithWrongEnnftTokenNameDatum,
  registerWithWrongEnopNftCurrencySymbolInDatum,
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
        "3.0 violation - Registration datum Output should be valid"
        [ testProperty "3.0.1.o violation - `ennftTokenName` field should be equal to the spent ennft token Name" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_3_0_1_o_Registration_Datum_Field_ENNFT_TokenName_Not_Valid
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ registerWithWrongEnnftTokenNameDatum
                    substrateKeyPair
                    (NFT ennftCurrencySymbol firstEnnftTn)
                    (NFT ennftCurrencySymbol (NL.head ennftsTokenNames))
                    commission
                    operator
        , testProperty "3.0.2.o violation - `ennopNftCurrencySymbol` field should be equal to the spent ennop nft currency symbol" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_3_0_2_o_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid
                  (registrationCookedConfig ennftCurrencySymbol)
                  genesis
                  $ registerWithWrongEnopNftCurrencySymbolInDatum
                    substrateKeyPair
                    (NFT ennftCurrencySymbol firstEnnftTn)
                    anotherDifferentENNftCurrencySymbol
                    commission
                    operator
        ]
    , testProperty "3.2.0.o violation - Registration datum Output is not Auhtentic" $
        forAll (genFixtureWithInvalidSignature keys) $
          \FixtureWithInvalidSignature{..} ->
            shouldViolateAProperty
              v_3_2_o_Registration_Validator_Output_Datum_Not_Authentic
              (registrationCookedConfig . currencySymbol $ ennft)
              genesis
              $ registerWithInvalidDatumVerification substrateKeyPair ennft commission operator invalidSignature
    , testProperty "3.3.0.o violation - Registration validator Output not found (not verifiable)" $
        forAll (genFixtureNominalCase keys) $
          \FixtureNominalCase{..} ->
            shouldViolateAProperty
              v_3_3_o_Registration_Validator_Output_NotFound
              (registrationCookedConfig . currencySymbol $ ennft)
              genesis
              $ mintWithoutRegistrationScript substrateKeyPair ennft operator
    , testProperty "3.4.0.o violation - Registration validator Output has no datum (not verifiable) " $
        forAll (genFixtureNominalCase keys) $
          \FixtureNominalCase{..} ->
            shouldViolateAProperty
              v_3_4_o_Registration_Validator_Output_Has_No_Datum
              (registrationCookedConfig . currencySymbol $ ennft)
              genesis
              $ registerWithNoRegistrationDatum substrateKeyPair ennft commission operator
    , testProperty "3.5.0.o violation - Registration validator Output has only the hashed datum (not verifiable)" $
        forAll (genFixtureNominalCase keys) $
          \FixtureNominalCase{..} ->
            shouldViolateAProperty
              v_3_5_o_Registration_Validator_Output_Has_Only_Hashed_Datum
              (registrationCookedConfig . currencySymbol $ ennft)
              genesis
              $ registerWithHashedRegistrationDatum substrateKeyPair ennft commission operator
    , testProperty "3.6.0.o violation - More than one Registration validator Output is not allowed (Reducing complexity)" $
        forAll (genFixtureMultipleENNFTs keys) $
          \FixtureMultipleENNFTs{..} ->
            shouldViolateAProperty
              v_3_6_o_More_Than_1_Registration_Validator_Output
              (registrationCookedConfig ennftCurrencySymbol)
              genesis
              $ registerMoreThanOneOperator
                substrateKeyPair
                (ennftCurrencySymbol, firstEnnftTn, NL.head ennftsTokenNames)
                commission
                operator
    ]
