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

module Aya.Registration.Core.Property.NFT.Ownership.Register (
  mk_r_Ownerhip_ENOP_NFT,
  -- `Property 2.0` : ENOP NFT should be given to the operator
  v_r_2_0_0_ENNOP_Minted_Not_Output_To_Operator,
  -- `Property 2.1` : ENNFT should be output on script (Already Validated by - `1.0.2 violation` - No ENNFT on Registration validator output  )
  -- `Property 2.2` : Only the operator should sign the transaction
  -- `2.1.0 violation` - No signer found (Enforced by Ledger Properties)
) where

import Aya.Registration.Core.Property.NFT.Transitivity.Register (
  v_r_1_0_1_ENNOP_Minted_Quantity_Above_One,
  v_r_1_1_1_ENOP_NFT_Cardinality_Above_1,
 )
import Aya.Registration.Core.Property.Violation
import PlutusTx.Prelude (BuiltinString)

{-# INLINEABLE v_r_2_0_0_ENNOP_Minted_Not_Output_To_Operator #-}
v_r_2_0_0_ENNOP_Minted_Not_Output_To_Operator :: BuiltinString
v_r_2_0_0_ENNOP_Minted_Not_Output_To_Operator = "r.2.0.0"

{-# INLINEABLE mk_r_Ownerhip_ENOP_NFT #-}
mk_r_Ownerhip_ENOP_NFT :: NFTPropertyViolationMsg
mk_r_Ownerhip_ENOP_NFT =
  NFTPropertyViolationMsg
    { whenNoNFT = v_r_2_0_0_ENNOP_Minted_Not_Output_To_Operator
    , whenQuantityMoreThanOne = v_r_1_0_1_ENNOP_Minted_Quantity_Above_One
    , whenMultipleTokenNamesForSameCurrencySymbol = v_r_1_1_1_ENOP_NFT_Cardinality_Above_1
    }
