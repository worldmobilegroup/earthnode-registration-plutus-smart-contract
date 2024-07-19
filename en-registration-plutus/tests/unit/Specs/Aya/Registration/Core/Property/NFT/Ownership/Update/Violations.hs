{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.NFT.Ownership.Update.Violations (
  specs,
) where

import Test.Tasty (TestTree, testGroup)

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)

import Specs.Aya.Registration.Core.Register.Fixture
import Specs.Aya.Registration.Core.Register.TxBuilding

import Adapter.Cooked
import Aya.Registration.Core.Property.NFT.Ownership.UniqueSigner (v_2_2_1_More_Than_One_Signer)
import Specs.Aya.Registration.Core.Model
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
        "Property 2.0` - ENOP NFT should be spent and given back to the operator"
        [ testProperty "u.2.0.0 violation - No ENNOP as Input (Already Validated by - `u.1.0.0 violation` )" True
        ]
    , testProperty
        "Property 2.1 - ENNFT should be output on script (Already Validated by - `u.1.0.2 violation` - No ENNFT on Registration validator output)"
        True
    , testGroup
        "Property 2.1 : Only the operator should sign the transaction"
        [ testProperty
            "2.1.0 violation - No signer found (Enforced by Ledger Properties)"
            True
        , testProperty "2.1.1 violation - signer is not unique" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_2_2_1_More_Than_One_Signer
                  (registrationCookedConfig . currencySymbol $ ennft)
                  genesis
                  $ do
                    registrationReference <- register substrateKeyPair ennft commission operator
                    updateWith2WalletSigning substrateKeyPair registrationReference commission operator anotherOperator
        ]
    ]
