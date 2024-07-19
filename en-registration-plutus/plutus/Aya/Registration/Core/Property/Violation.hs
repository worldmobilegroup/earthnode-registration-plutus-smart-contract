{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- Options
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Aya.Registration.Core.Property.Violation (
  NFTPropertyViolationMsg (..),
) where

import PlutusTx.Prelude (BuiltinString)

data NFTPropertyViolationMsg = NFTPropertyViolationMsg
  { whenNoNFT :: BuiltinString
  , whenQuantityMoreThanOne :: BuiltinString
  , whenMultipleTokenNamesForSameCurrencySymbol :: BuiltinString
  }
