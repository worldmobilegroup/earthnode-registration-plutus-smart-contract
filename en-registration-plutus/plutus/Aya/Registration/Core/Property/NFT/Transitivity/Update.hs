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
-- Options
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

{-# HLINT ignore "Use camelCase" #-}

module Aya.Registration.Core.Property.NFT.Transitivity.Update (
  mk_u_ENOP_NFT_msgs,
  mk_u_EN_NFT_msgs,
  -- `Property 1.0` : Tokens Quantities are verified
  v_u_1_0_0_ENNOP_Not_Output_to_Operator,
  v_u_1_0_1_No_ENNFT_On_Validator_Output,
  v_u_1_0_2_No_Minting_Allowed,
  v_u_1_0_3_No_Burning_Allowed,
  v_u_1_0_4_ENNFT_Quantity_Above_One,
  -- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
  v_u_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName,
  v_u_1_1_1_ENOP_NFT_Cardinality_Above_1,
  v_u_1_1_2_ENNFT_Cardinality_Above_1,
) where

import Aya.Registration.Core.Property.Violation
import PlutusTx.Prelude (BuiltinString)

-- `Property 1.0` : Tokens Quantities are verified
{-# INLINEABLE v_u_1_0_0_ENNOP_Not_Output_to_Operator #-}
{-# INLINEABLE v_u_1_0_1_No_ENNFT_On_Validator_Output #-}
{-# INLINEABLE v_u_1_0_2_No_Minting_Allowed #-}
{-# INLINEABLE v_u_1_0_3_No_Burning_Allowed #-}
{-# INLINEABLE v_u_1_0_4_ENNFT_Quantity_Above_One #-}

-- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
{-# INLINEABLE v_u_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName #-}
{-# INLINEABLE v_u_1_1_1_ENOP_NFT_Cardinality_Above_1 #-}
{-# INLINEABLE v_u_1_1_2_ENNFT_Cardinality_Above_1 #-}
-- `Property 1.0` : Tokens Quantities are verified
v_u_1_0_0_ENNOP_Not_Output_to_Operator
  , v_u_1_0_1_No_ENNFT_On_Validator_Output
  , v_u_1_0_2_No_Minting_Allowed
  , v_u_1_0_3_No_Burning_Allowed
  , v_u_1_0_4_ENNFT_Quantity_Above_One
  , -- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
  v_u_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName
  , v_u_1_1_1_ENOP_NFT_Cardinality_Above_1
  , v_u_1_1_2_ENNFT_Cardinality_Above_1
    :: BuiltinString
-- `Property 1.0` : Tokens Quantities are verified
v_u_1_0_0_ENNOP_Not_Output_to_Operator = "u.1.0.0"
v_u_1_0_1_No_ENNFT_On_Validator_Output = "u.1.0.1"
v_u_1_0_2_No_Minting_Allowed = "u.1.0.2"
v_u_1_0_3_No_Burning_Allowed = "u.1.0.3"
v_u_1_0_4_ENNFT_Quantity_Above_One = "u.1.0.4"

-- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
v_u_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName = "u.1.1.0"
v_u_1_1_1_ENOP_NFT_Cardinality_Above_1 = "u.1.1.1"
v_u_1_1_2_ENNFT_Cardinality_Above_1 = "u.1.1.2"

{-# INLINEABLE mk_u_ENOP_NFT_msgs #-}
mk_u_ENOP_NFT_msgs :: NFTPropertyViolationMsg
mk_u_ENOP_NFT_msgs =
  NFTPropertyViolationMsg
    { whenNoNFT = v_u_1_0_0_ENNOP_Not_Output_to_Operator
    , whenQuantityMoreThanOne = "can't happen"
    , whenMultipleTokenNamesForSameCurrencySymbol = v_u_1_1_1_ENOP_NFT_Cardinality_Above_1
    }

{-# INLINEABLE mk_u_EN_NFT_msgs #-}
mk_u_EN_NFT_msgs :: NFTPropertyViolationMsg
mk_u_EN_NFT_msgs =
  NFTPropertyViolationMsg
    { whenNoNFT = v_u_1_0_1_No_ENNFT_On_Validator_Output
    , whenQuantityMoreThanOne = v_u_1_0_4_ENNFT_Quantity_Above_One
    , whenMultipleTokenNamesForSameCurrencySymbol = v_u_1_1_2_ENNFT_Cardinality_Above_1
    }
