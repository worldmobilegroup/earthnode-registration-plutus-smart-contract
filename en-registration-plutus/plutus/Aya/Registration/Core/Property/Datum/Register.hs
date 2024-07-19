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

module Aya.Registration.Core.Property.Datum.Register (
  v_3_0_1_i_Registration_Datum_Field_ENNFT_TokenName_Not_Valid,
  v_3_0_2_i_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid,
  v_3_1_i_Registration_Input_Datum_Not_Deserializable,
  v_3_2_i_Registration_Validator_Input_Datum_Not_Authentic,
  v_3_3_i_Registration_Validator_Input_NotFound,
  v_3_4_i_Registration_Validator_Input_Has_No_Datum,
  v_3_5_i_Registration_Validator_Input_Has_Only_Hashed_Datum,
  v_3_6_i_More_Than_1_Registration_Validator_Input,
  v_3_0_1_o_Registration_Datum_Field_ENNFT_TokenName_Not_Valid,
  v_3_0_2_o_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid,
  v_3_1_o_Registration_Output_Datum_Not_Deserializable,
  v_3_2_o_Registration_Validator_Output_Datum_Not_Authentic,
  v_3_3_o_Registration_Validator_Output_NotFound,
  v_3_4_o_Registration_Validator_Output_Has_No_Datum,
  v_3_5_o_Registration_Validator_Output_Has_Only_Hashed_Datum,
  v_3_6_o_More_Than_1_Registration_Validator_Output,
) where

import PlutusTx.Prelude (BuiltinString)

{-# INLINEABLE v_3_0_1_i_Registration_Datum_Field_ENNFT_TokenName_Not_Valid #-}
{-# INLINEABLE v_3_0_2_i_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid #-}
{-# INLINEABLE v_3_1_i_Registration_Input_Datum_Not_Deserializable #-}
{-# INLINEABLE v_3_2_i_Registration_Validator_Input_Datum_Not_Authentic #-}
{-# INLINEABLE v_3_3_i_Registration_Validator_Input_NotFound #-}
{-# INLINEABLE v_3_4_i_Registration_Validator_Input_Has_No_Datum #-}
{-# INLINEABLE v_3_5_i_Registration_Validator_Input_Has_Only_Hashed_Datum #-}
{-# INLINEABLE v_3_6_i_More_Than_1_Registration_Validator_Input #-}
{-# INLINEABLE v_3_0_1_o_Registration_Datum_Field_ENNFT_TokenName_Not_Valid #-}
{-# INLINEABLE v_3_0_2_o_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid #-}
{-# INLINEABLE v_3_1_o_Registration_Output_Datum_Not_Deserializable #-}
{-# INLINEABLE v_3_2_o_Registration_Validator_Output_Datum_Not_Authentic #-}
{-# INLINEABLE v_3_3_o_Registration_Validator_Output_NotFound #-}
{-# INLINEABLE v_3_4_o_Registration_Validator_Output_Has_No_Datum #-}
{-# INLINEABLE v_3_5_o_Registration_Validator_Output_Has_Only_Hashed_Datum #-}
{-# INLINEABLE v_3_6_o_More_Than_1_Registration_Validator_Output #-}
v_3_0_1_i_Registration_Datum_Field_ENNFT_TokenName_Not_Valid
  , v_3_0_2_i_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid
  , v_3_1_i_Registration_Input_Datum_Not_Deserializable
  , v_3_2_i_Registration_Validator_Input_Datum_Not_Authentic
  , v_3_3_i_Registration_Validator_Input_NotFound
  , v_3_4_i_Registration_Validator_Input_Has_No_Datum
  , v_3_5_i_Registration_Validator_Input_Has_Only_Hashed_Datum
  , v_3_6_i_More_Than_1_Registration_Validator_Input
  , v_3_0_1_o_Registration_Datum_Field_ENNFT_TokenName_Not_Valid
  , v_3_0_2_o_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid
  , v_3_1_o_Registration_Output_Datum_Not_Deserializable
  , v_3_2_o_Registration_Validator_Output_Datum_Not_Authentic
  , v_3_3_o_Registration_Validator_Output_NotFound
  , v_3_4_o_Registration_Validator_Output_Has_No_Datum
  , v_3_5_o_Registration_Validator_Output_Has_Only_Hashed_Datum
  , v_3_6_o_More_Than_1_Registration_Validator_Output
    :: BuiltinString
v_3_0_1_i_Registration_Datum_Field_ENNFT_TokenName_Not_Valid = "3.0.1.i"
v_3_0_2_i_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid = "3.0.2.i"
v_3_1_i_Registration_Input_Datum_Not_Deserializable = "3.1.i"
v_3_2_i_Registration_Validator_Input_Datum_Not_Authentic = "3.2.i"
v_3_3_i_Registration_Validator_Input_NotFound = "3.3.i"
v_3_4_i_Registration_Validator_Input_Has_No_Datum = "3.4.i"
v_3_5_i_Registration_Validator_Input_Has_Only_Hashed_Datum = "3.5.i"
v_3_6_i_More_Than_1_Registration_Validator_Input = "3.6.i"

v_3_0_1_o_Registration_Datum_Field_ENNFT_TokenName_Not_Valid = "3.0.1.o"
v_3_0_2_o_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid = "3.0.2.o"
v_3_1_o_Registration_Output_Datum_Not_Deserializable = "3.1.o"
v_3_2_o_Registration_Validator_Output_Datum_Not_Authentic = "3.2.o"
v_3_3_o_Registration_Validator_Output_NotFound = "3.3.o"
v_3_4_o_Registration_Validator_Output_Has_No_Datum = "3.4.o"
v_3_5_o_Registration_Validator_Output_Has_Only_Hashed_Datum = "3.5.o"
v_3_6_o_More_Than_1_Registration_Validator_Output = "3.6.o"
