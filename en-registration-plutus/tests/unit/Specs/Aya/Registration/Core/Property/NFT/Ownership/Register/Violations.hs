{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.NFT.Ownership.Register.Violations (
  specs,
) where

import Test.Tasty (TestTree, testGroup)

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)

import Aya.Registration.Core.Property.NFT.Ownership.Register
import Specs.Aya.Registration.Core.Register.Fixture
import Specs.Aya.Registration.Core.Register.TxBuilding

import Adapter.Cooked
import Aya.Registration.Core.Property.NFT.Ownership.UniqueSigner
import Specs.Aya.Registration.Core.Model
import Test.Tasty.QuickCheck (
  forAll,
  testProperty,
 )

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
  testGroup
    "Register"
    [ testGroup
        "Property 2.0 - ENOP NFT should be given to the operator"
        [ testProperty "2.0.0 violation - ENNOP Minted Not Output to Operator" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_r_2_0_0_ENNOP_Minted_Not_Output_To_Operator
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ registerAndMintToAnotherOperator substrateKeyPair ennft commission operator anotherOperator
        ]
    , testProperty
        "Property 2.1 - ENNFT should be output on script (Already Validated by - `1.0.2 violation` - No ENNFT on Registration validator output  )"
        True
    , testGroup
        "Property 2.2 - Only the operator should sign the transaction"
        [ testProperty
            "2.2.0 violation - No signer found (Enforced by Ledger Properties)"
            True
        , testProperty "2.2.1 violation - signer is not unique" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_2_2_1_More_Than_One_Signer
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ registerWith2WalletsSigning substrateKeyPair ennft commission operator anotherOperator
        ]
    ]
