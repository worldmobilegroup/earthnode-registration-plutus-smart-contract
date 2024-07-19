{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Register.NominalCase (
  specs,
) where

import Cooked (testSucceedsFrom)
import Test.Tasty (TestTree)

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)
import Data.Default (Default (def))

import Specs.Aya.Registration.Core.Register.Fixture

import Specs.Aya.Registration.Core.Model
import Specs.Aya.Registration.Core.Register.TxBuilding (register)
import Test.Tasty.QuickCheck

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
  testProperty "Register - Operators can Register their Aya Node On Cardano" $
    forAll (genFixtureNominalCase keys) $
      \FixtureNominalCase{..} ->
        testSucceedsFrom @Property
          def
          genesis
          $ register substrateKeyPair ennft commission operator
