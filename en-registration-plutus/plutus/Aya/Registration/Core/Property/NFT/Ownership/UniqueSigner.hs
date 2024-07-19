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

module Aya.Registration.Core.Property.NFT.Ownership.UniqueSigner (
  -- `Property 2.2` : Only the operator should sign the transaction
  -- `2.1.0 violation` - No signer found (Enforced by Ledger Properties)
  v_2_2_1_More_Than_One_Signer,
) where

import PlutusTx.Prelude (BuiltinString)

-- `Property 2.2` : Only the operator should sign the transaction
{-# INLINEABLE v_2_2_1_More_Than_One_Signer #-}
v_2_2_1_More_Than_One_Signer :: BuiltinString
v_2_2_1_More_Than_One_Signer = "2.2.1"
