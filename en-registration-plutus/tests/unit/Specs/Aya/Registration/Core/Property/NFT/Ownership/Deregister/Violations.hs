{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.NFT.Ownership.Deregister.Violations (
  specs,
) where

import Test.Tasty (TestTree, testGroup)

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)

import Aya.Registration.Core.Property.NFT.Ownership.Register

import Specs.Aya.Registration.Core.Register.Fixture
import Specs.Aya.Registration.Core.Register.TxBuilding

import Adapter.Cooked
import Aya.Registration.Core.Property.NFT.Ownership.Deregister (
  v_d_2_0_1_Burn_Without_A_Proper_Registration_Validator_Input,
 )
import Aya.Registration.Core.Property.NFT.Ownership.UniqueSigner (v_2_2_1_More_Than_One_Signer)
import Specs.Aya.Registration.Core.Deregister.TxBuilding (
  deregisterWihtoutRegistrationValidatorInput,
  deregisterWith2WalletSigning,
 )
import Specs.Aya.Registration.Core.Model
import Specs.Aya.Registration.Core.Update.TxBuilding (update)
import Test.Tasty.QuickCheck (
  forAll,
  testProperty,
 )

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
  testGroup
    "Deregister"
    [ testGroup
        "Property 2.0 - ENOP NFT should be spent from the operator and burnt"
        [ testProperty "d.2.0.0 violation - No ENNOP Burn (Already Validated by - `d.1.0.0 violation` )" True
        , testProperty "d.2.0.1 violation - Burn Without A Proper Registration Validator Input Not Allowed" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_d_2_0_1_Burn_Without_A_Proper_Registration_Validator_Input
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair ennft commission operator
                    registrationUpdateReference <- update substrateKeyPair registrationReference commission operator anotherOperator
                    deregisterWihtoutRegistrationValidatorInput registrationUpdateReference operator
        ]
    , testProperty
        "Property 2.1 : ENNFT should be spent from Registration scipt and output to Operator (Already Validated by - `d.1.0.1 violation` )"
        True
    , testGroup
        "Property 2.1 : Only the operator should sign the transaction"
        [ testProperty
            "d.2.1.0 violation - No signer found (Enforced by Ledger Properties)"
            True
        , testProperty "d.2.1.1 violation - signer is not unique" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_2_2_1_More_Than_One_Signer
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair ennft commission operator
                    deregisterWith2WalletSigning registrationReference operator anotherOperator
        ]
    ]
