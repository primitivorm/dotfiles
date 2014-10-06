-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Date
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A date plugin for Xmobar
--
-- Usage example: in template put
--
-- > Run Date "%a %b %_d %Y <fc=#ee9a00> %H:%M:%S</fc>" "Mydate" 10
--
-----------------------------------------------------------------------------

module Plugins.Date where

import Plugins

import System.Locale
import System.Time

data Date = Date String String Int
    deriving (Read, Show)

instance Exec Date where
    alias (Date _ a _) = a
    run   (Date f _ _) = date f
    rate  (Date _ _ r) = r

date :: String -> IO String
date format = do
  t <- toCalendarTime =<< getClockTime
  return $ formatCalendarTime defaultTimeLocale format t
