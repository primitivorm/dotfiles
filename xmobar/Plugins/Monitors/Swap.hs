-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Swap
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A  swap usage monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Swap where

import Plugins.Monitors.Common

import qualified Data.ByteString.Lazy.Char8 as B

swapConfig :: IO MConfig
swapConfig = mkMConfig
        "Swap: <usedratio>"                    -- template
        ["total", "used", "free", "usedratio"] -- available replacements

fileMEM :: IO B.ByteString
fileMEM = B.readFile "/proc/meminfo"

parseMEM :: IO [Float]
parseMEM =
    do file <- fileMEM
       let li i l
               | l /= [] = (head l) !! i
               | otherwise = B.empty
           fs s l
               | l == []    = False
               | otherwise  = head l == B.pack s
           get_data s = flip (/) 1024 . read . B.unpack . li 1 . filter (fs s)
           st   = map B.words . B.lines $ file
           tot  = get_data "SwapTotal:" st
           free = get_data "SwapFree:" st
       return [tot, (tot - free), free, (tot - free) / tot]

formatSwap :: [Float] -> Monitor [String]
formatSwap x =
    do let f1 n = showDigits 2 n
           (hd, tl) = splitAt 3 x
       firsts <- mapM (showWithColors f1) hd
       lasts <- showPercentsWithColors (map (/100) tl)
       return $ firsts ++ lasts

runSwap :: [String] -> Monitor String
runSwap _ =
    do m <- io $ parseMEM
       l <- formatSwap m
       parseTemplate l
