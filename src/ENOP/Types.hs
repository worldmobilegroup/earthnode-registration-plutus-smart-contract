{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v2.0
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-specialise            #-}
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
{-# LANGUAGE BangPatterns          #-}

module ENOP.Types where

import           Data.Aeson           (FromJSON, ToJSON)
import           Ledger
import           GHC.Generics         (Generic)
import qualified PlutusTx
import qualified Prelude              

data ScriptParams = ScriptParams
        {
              spNftCs            :: CurrencySymbol
            , spRegContr         :: ValidatorHash
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.unstableMakeIsData ''ScriptParams
PlutusTx.makeLift ''ScriptParams

data Action = Mint | Burn
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''Action [('ENOP.Types.Mint, 0),('Burn, 1)]
PlutusTx.makeLift ''Action
