{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (
  main,
) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)

import qualified Specs.Aya.Registration.Core.Property.Datum.Register.Violations as Property.Datum.Register
import qualified Specs.Aya.Registration.Core.Property.Datum.Update.Violations as Property.Datum.Update

import qualified Specs.Aya.Registration.Core.Deregister.NominalCase as Deregister.Nominal.Case
import qualified Specs.Aya.Registration.Core.Property.NFT.Ownership.Deregister.Violations as Property.NFT.Ownership.Deregister
import qualified Specs.Aya.Registration.Core.Property.NFT.Ownership.Register.Violations as Property.NFT.Ownership.Register
import qualified Specs.Aya.Registration.Core.Property.NFT.Ownership.Update.Violations as Property.NFT.Ownership.Update
import qualified Specs.Aya.Registration.Core.Property.NFT.Transitivity.Deregister.Violations as Property.NFT.Transitivity.Deregister
import qualified Specs.Aya.Registration.Core.Property.NFT.Transitivity.Register.Violations as Property.NFT.Transitivity.Register
import qualified Specs.Aya.Registration.Core.Property.NFT.Transitivity.Update.Violations as Property.NFT.Transitivity.Update
import qualified Specs.Aya.Registration.Core.Register.NominalCase as Register.Nominal.Case
import qualified Specs.Aya.Registration.Core.Update.NominalCase as Update.Nominal.Case
import Test.Tasty (defaultMain, testGroup)

import Cardano.Crypto.DSIGN

import Adapter.CardanoCryptoClass.Crypto
import Data.Aeson hiding (decode, decode', encode)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  keys <- fromJust <$> decodeFileStrict @[KeyPair Ed25519DSIGN] "tests/unit/data/generatedSubstrateEd25519Keys.json"
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "Validator Registration Plutus Contract - Unit Tests"
      [ testGroup
          "On Chain Specifications"
          [ testGroup
              "Nominal Cases"
              [ Register.Nominal.Case.specs keys
              , Update.Nominal.Case.specs keys
              , Deregister.Nominal.Case.specs keys
              ]
          , testGroup
              "Property 1 : Non Fungible Property Transitivity : EN Token is an NFT => the ENOP Token should be an NFT, and ENOP NFT only exist when the EN NFT is locked on script"
              [ Property.NFT.Transitivity.Register.specs keys
              , Property.NFT.Transitivity.Update.specs keys
              , Property.NFT.Transitivity.Deregister.specs keys
              ]
          , testGroup
              "Property 2 - Preserving NFTs ownership : ENOP and ENNFT can be swapped only between the operator and the registration smart contract"
              [ Property.NFT.Ownership.Register.specs keys
              , Property.NFT.Ownership.Update.specs keys
              , Property.NFT.Ownership.Deregister.specs keys
              ]
          , testGroup
              "Property 3 - Datum Authenticity & Validity : The registration details (datum) should be verifiable and Valid (Signed and Provided by the owner of the ENNFT)"
              [ Property.Datum.Register.specs keys
              , Property.Datum.Update.specs keys
              , Property.NFT.Ownership.Deregister.specs keys
              ]
          ]
      , testGroup
          "Off Chain"
          []
      ]
