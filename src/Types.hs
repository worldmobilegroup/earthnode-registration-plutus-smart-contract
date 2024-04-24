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
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-specialise            #-}
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
{-# LANGUAGE BangPatterns          #-}

module Types where

import           Data.Aeson           (FromJSON, ToJSON)
import           Ledger
import           Playground.Contract  (Generic)
import           Plutus.V2.Ledger.Api
import qualified PlutusTx
import           PlutusTx.Prelude     (Integer)
import qualified Prelude

data ScriptParams = ScriptParams
        {
              pNftCs :: CurrencySymbol -- ^ CurrencySymbol of the ENNFT
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.unstableMakeIsData ''ScriptParams
PlutusTx.makeLift ''ScriptParams

data EnRegistration = EnRegistration
        {
              enOperatorAddress :: BuiltinByteString -- ^ TODO Address of what exactly? Cardano address?
            , enConsensusPubKey :: BuiltinByteString -- ^ TODO What is that?
            , enMerkleTreeRoot  :: BuiltinByteString -- ^ TODO What is this used for?
            , enCceAddress      :: BuiltinByteString -- ^ TODO What is CCE?
            , enUsedNftTn       :: TokenName -- ^ TODO Why "used"? This is the ENNFT right? And IIUC, the token name is specific to the operator?
            , enRwdWallet       :: PubKeyHash -- ^ TODO So that is the wallet the operator should get the rewards after participating in block production in Aya?
            , enCommission      :: Integer -- ^ TODO What is that exactly?
            , pEnOpCs           :: CurrencySymbol -- We cannot store the EnOpNft CurrencySymbol in the parameter because we get a cyclic dependency
            , enSignature       :: BuiltinByteString -- ^ TODO Signature of what exactly?
        } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''EnRegistration [('EnRegistration, 0)]
PlutusTx.makeLift ''EnRegistration

data Action = Unregister | Update BuiltinByteString
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''Action [('Unregister, 0),('Update, 1)]
PlutusTx.makeLift ''Action
