{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v2.0
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}

import           Data.ByteString as BS
import           Data.Word8 as W8
import           Cardano.Api
import           OffChain
import           Plutus.Contract.CardanoAPI   (toCardanoAddressInEra)
import           Plutus.V1.Ledger.Value       (currencySymbol)
import           Prelude
import           System.Environment           (getArgs)
import           ENOP.Types
import           Types
import           ENOP.MpOffChain

main :: IO ()
main = do
    args <- getArgs
    case args of

      [magic'] -> do
        let
            val_scriptFile  = "en-nft-registration-v2_test_ENNFTs_4c5ac6739376849c917d299a4ef3c74b44cfb1a0ebd4948877058559.plutus"
            mp_scriptFile   = "enop-nft-mintingPolicy-v1_test_ENNFTs_4c5ac6739376849c917d299a4ef3c74b44cfb1a0ebd4948877058559.plutus"
            -- ENNFT Policy in Bytes
            cs = currencySymbol $ BS.pack ([76,90,198,115,147,118,132,156,145,125,41,154,78,243,199,75,68,207,177,160,235,212,148,136,119,5,133,89::W8.Word8])
            -- 4C5AC6739376849C917D299A4EF3C74B44CFB1A0EBD4948877058559
            -- 4c5ac6739376849c917d299a4ef3c74b44cfb1a0ebd4948877058559
            -- Public Testnet ENNFT ([216,190,188,176,171,216,145,147,135,76,89,237,48,35,245,180,248,27,137,182,103,109,24,122,215,251,219,14::W8.Word8])
            val_params          = Types.ScriptParams
                {
                    pNftCs       = cs
                }
            magic = case read magic' :: Integer of        -- 1..n: (Testnet (NetworkMagic n)) || 0: Mainnet
              0 -> Mainnet
              x -> Testnet (NetworkMagic $ fromInteger x)
            address     = scriptAddress val_params
            vh_reg      = OffChain.scriptHash val_params
            mp_params   = ENOP.Types.ScriptParams
                {
                    spNftCs            = cs
                  , spRegContr         = vh_reg
                }
            address'    = case toCardanoAddressInEra magic address of
                            Left err    -> error $ "cannot create bech32 contract address: " ++ show err
                            Right addr' -> serialiseAddress addr'
            cs_mp       = mustMintPolicyCurrencySymbol mp_params
        scriptResultValidator <- writeFileTextEnvelope val_scriptFile Nothing $ apiScript val_params
        scriptResultMintingPolicy <- writeFileTextEnvelope mp_scriptFile Nothing $ mPapiScript mp_params
        case scriptResultValidator of
            Left err -> print $ displayError err
            Right () -> Prelude.putStrLn $ "{\"plutus_file\":" ++ show val_scriptFile ++ ",\"script_address\":" ++ show address' ++ "}"
        case scriptResultMintingPolicy of
            Left err -> print $ displayError err
            Right () -> Prelude.putStrLn $ "{\"plutus_file\":" ++ show mp_scriptFile ++ ",\"currency_symbol\":\"" ++ show cs_mp ++ "\"}"
      _ -> error "You need to provide the networkmagic (0 == mainnet)!"