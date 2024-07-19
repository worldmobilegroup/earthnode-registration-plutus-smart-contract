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

module Aya.Registration.Core.Property.NFT.Transitivity.Deregister (
  mk_d_EN_NFT_msgs,
  mk_d_ENOP_NFT_msgs,
  -- `Property 1.0` : Tokens Quantities are verified
  v_d_1_0_0_No_ENNOP_To_Burn,
  v_d_1_0_1_No_Minting_Allowed,
  v_d_1_0_2_More_Than_One_ENOP_To_Burn,
  v_d_1_0_3_No_ENNFT_Released_To_Operator,
  -- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
  v_d_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName,
  v_d_1_1_1_ENOP_NFT_Cardinality_Above_1,
  v_d_1_1_2_ENNFT_Cardinality_Above_1,
  v_d_1_1_3_No_Registration_Validator_Output_Allowed,
) where

import Aya.Registration.Core.Property.Violation
import PlutusTx.Prelude (BuiltinString)

-- `Property 1.0` : Tokens Quantities are verified
{-# INLINEABLE v_d_1_0_0_No_ENNOP_To_Burn #-}
{-# INLINEABLE v_d_1_0_1_No_Minting_Allowed #-}
{-# INLINEABLE v_d_1_0_2_More_Than_One_ENOP_To_Burn #-}
{-# INLINEABLE v_d_1_0_3_No_ENNFT_Released_To_Operator #-}

-- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
{-# INLINEABLE v_d_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName #-}
{-# INLINEABLE v_d_1_1_1_ENOP_NFT_Cardinality_Above_1 #-}
{-# INLINEABLE v_d_1_1_2_ENNFT_Cardinality_Above_1 #-}
{-# INLINEABLE v_d_1_1_3_No_Registration_Validator_Output_Allowed #-}
-- `Property 1.0` : Tokens Quantities are verified
v_d_1_0_0_No_ENNOP_To_Burn
  , v_d_1_0_1_No_Minting_Allowed
  , v_d_1_0_2_More_Than_One_ENOP_To_Burn
  , v_d_1_0_3_No_ENNFT_Released_To_Operator
  , -- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
  v_d_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName
  , v_d_1_1_1_ENOP_NFT_Cardinality_Above_1
  , v_d_1_1_2_ENNFT_Cardinality_Above_1
  , v_d_1_1_3_No_Registration_Validator_Output_Allowed
    :: BuiltinString
-- `Property 1.0` : Tokens Quantities are verified
v_d_1_0_0_No_ENNOP_To_Burn = "d.1.0.0"
v_d_1_0_1_No_Minting_Allowed = "d.1.0.1"
v_d_1_0_2_More_Than_One_ENOP_To_Burn = "d.1.0.2"
v_d_1_0_3_No_ENNFT_Released_To_Operator = "d.1.0.3"

-- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
v_d_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName = "d.1.1.0"
v_d_1_1_1_ENOP_NFT_Cardinality_Above_1 = "d.1.1.1"
v_d_1_1_2_ENNFT_Cardinality_Above_1 = "d.1.1.2"
v_d_1_1_3_No_Registration_Validator_Output_Allowed = "d.1.1.3"

{-# INLINEABLE mk_d_EN_NFT_msgs #-}
mk_d_EN_NFT_msgs :: NFTPropertyViolationMsg
mk_d_EN_NFT_msgs =
  NFTPropertyViolationMsg
    { whenNoNFT = v_d_1_0_3_No_ENNFT_Released_To_Operator
    , whenQuantityMoreThanOne = "EN Token Fungible"
    , whenMultipleTokenNamesForSameCurrencySymbol = v_d_1_1_2_ENNFT_Cardinality_Above_1
    }

{-# INLINEABLE mk_d_ENOP_NFT_msgs #-}
mk_d_ENOP_NFT_msgs :: NFTPropertyViolationMsg
mk_d_ENOP_NFT_msgs =
  NFTPropertyViolationMsg
    { whenNoNFT = v_d_1_0_0_No_ENNOP_To_Burn
    , whenQuantityMoreThanOne = "ENOP Token Fungible"
    , whenMultipleTokenNamesForSameCurrencySymbol = v_d_1_1_1_ENOP_NFT_Cardinality_Above_1
    }
