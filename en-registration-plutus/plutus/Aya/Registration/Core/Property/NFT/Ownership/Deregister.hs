{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

{-# HLINT ignore "Use camelCase" #-}

module Aya.Registration.Core.Property.NFT.Ownership.Deregister (
  -- `Property 2.0 - ENOP NFT should be spent from the operator and burnt
  -- 2.0.0 violation - No ENNOP Burn (Already Validated by - `d.1.0.0 violation` ))
  v_d_2_0_1_Burn_Without_A_Proper_Registration_Validator_Input,
) where

import PlutusTx.Prelude (BuiltinString)

-- `Property 2.2` : Only the operator should sign the transaction
{-# INLINEABLE v_d_2_0_1_Burn_Without_A_Proper_Registration_Validator_Input #-}
v_d_2_0_1_Burn_Without_A_Proper_Registration_Validator_Input :: BuiltinString
v_d_2_0_1_Burn_Without_A_Proper_Registration_Validator_Input = "d.2.0.1"
