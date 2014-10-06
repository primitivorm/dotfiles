-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Plugins
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module exports the API for plugins.
--
-- Have a look at Plugins\/HelloWorld.hs
--
-----------------------------------------------------------------------------

module Plugins
    ( Exec (..)
    , tenthSeconds
    , readFileSafe
    , hGetLineSafe
    ) where

import Commands
import XUtil
