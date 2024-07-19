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

module Aya.Registration.Core.Property.NFT.Transitivity.Register (
  mk_r_ENOP_NFT_msgs,
  mk_r_EN_NFT_msgs,
  -- `Property 1.0` : Tokens Quantities are verified
  v_r_1_0_0_No_ENNOP_Minted,
  v_r_1_0_1_ENNOP_Minted_Quantity_Above_One,
  v_r_1_0_2_No_ENNFT_On_Validator_Output,
  v_r_1_0_3_ENNFT_Quantity_Above_One,
  -- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
  v_r_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName,
  v_r_1_1_1_ENOP_NFT_Cardinality_Above_1,
  v_r_1_1_2_ENNFT_Cardinality_Above_1,
) where

import Aya.Registration.Core.Property.Violation
import PlutusTx.Prelude (BuiltinString)

-- `Property 1.0` : Tokens Quantities are verified
{-# INLINEABLE v_r_1_0_0_No_ENNOP_Minted #-}
{-# INLINEABLE v_r_1_0_1_ENNOP_Minted_Quantity_Above_One #-}
{-# INLINEABLE v_r_1_0_2_No_ENNFT_On_Validator_Output #-}
{-# INLINEABLE v_r_1_0_3_ENNFT_Quantity_Above_One #-}

-- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
{-# INLINEABLE v_r_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName #-}
{-# INLINEABLE v_r_1_1_1_ENOP_NFT_Cardinality_Above_1 #-}
{-# INLINEABLE v_r_1_1_2_ENNFT_Cardinality_Above_1 #-}
-- `Property 1.0` : Tokens Quantities are verified
v_r_1_0_0_No_ENNOP_Minted
  , v_r_1_0_1_ENNOP_Minted_Quantity_Above_One
  , v_r_1_0_2_No_ENNFT_On_Validator_Output
  , v_r_1_0_3_ENNFT_Quantity_Above_One
  , -- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
  v_r_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName
  , v_r_1_1_1_ENOP_NFT_Cardinality_Above_1
  , v_r_1_1_2_ENNFT_Cardinality_Above_1
    :: BuiltinString
-- `Property 1.0` : Tokens Quantities are verified
v_r_1_0_0_No_ENNOP_Minted = "r.1.0.0"
v_r_1_0_1_ENNOP_Minted_Quantity_Above_One = "r.1.0.1"
v_r_1_0_2_No_ENNFT_On_Validator_Output = "r.1.0.2"
v_r_1_0_3_ENNFT_Quantity_Above_One = "r.1.0.3"

-- `Property 1.1` : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
v_r_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName = "r.1.1.0"
v_r_1_1_1_ENOP_NFT_Cardinality_Above_1 = "r.1.1.1"
v_r_1_1_2_ENNFT_Cardinality_Above_1 = "r.1.1.2"

{-# INLINEABLE mk_r_ENOP_NFT_msgs #-}
mk_r_ENOP_NFT_msgs :: NFTPropertyViolationMsg
mk_r_ENOP_NFT_msgs =
  NFTPropertyViolationMsg
    { whenNoNFT = v_r_1_0_0_No_ENNOP_Minted
    , whenQuantityMoreThanOne = v_r_1_0_1_ENNOP_Minted_Quantity_Above_One
    , whenMultipleTokenNamesForSameCurrencySymbol = v_r_1_1_1_ENOP_NFT_Cardinality_Above_1
    }

{-# INLINEABLE mk_r_EN_NFT_msgs #-}
mk_r_EN_NFT_msgs :: NFTPropertyViolationMsg
mk_r_EN_NFT_msgs =
  NFTPropertyViolationMsg
    { whenNoNFT = v_r_1_0_2_No_ENNFT_On_Validator_Output
    , whenQuantityMoreThanOne = v_r_1_0_3_ENNFT_Quantity_Above_One
    , whenMultipleTokenNamesForSameCurrencySymbol = v_r_1_1_2_ENNFT_Cardinality_Above_1
    }
