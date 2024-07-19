{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
-- Options
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

module Adapter.Plutus.OffChain (
    validatorToTypedValidator
) where

import Plutus.Script.Utils.Scripts qualified as Script
import Plutus.Script.Utils.Typed qualified as Script hiding (validatorHash)
import Plutus.Script.Utils.V3.Typed.Scripts.MonetaryPolicies qualified as Script


validatorToTypedValidator :: Script.Validator -> Script.TypedValidator a
validatorToTypedValidator val =
    Script.TypedValidator
        { Script.tvValidator = vValidator
        , Script.tvValidatorHash = vValidatorHash
        , Script.tvForwardingMPS = vMintingPolicy
        , Script.tvForwardingMPSHash = Script.mintingPolicyHash vMintingPolicy
        }
  where
    vValidator = Script.Versioned val Script.PlutusV3
    vValidatorHash = Script.validatorHash vValidator
    forwardingPolicy = Script.mkForwardingMintingPolicy vValidatorHash
    vMintingPolicy = Script.Versioned forwardingPolicy Script.PlutusV3
