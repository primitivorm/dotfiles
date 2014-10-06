-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.CpuFreq
-- Copyright   :  (c) Juraj Hercek
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Juraj Hercek <juhe_haskell@hck.sk>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A cpu frequency monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.CpuFreq where

import Plugins.Monitors.Common
import Plugins.Monitors.CoreCommon

-- |
-- Cpu frequency default configuration. Default template contains only one
-- core frequency, user should specify custom template in order to get more
-- cpu frequencies.
cpuFreqConfig :: IO MConfig
cpuFreqConfig = mkMConfig
       "Freq: <cpu0>" -- template
       (zipWith (++) (repeat "cpu") (map show [0 :: Int ..])) -- available
                                                              -- replacements

-- |
-- Function retrieves monitor string holding the cpu frequency (or frequencies)
runCpuFreq :: [String] -> Monitor String
runCpuFreq _ = do
    let dir = "/sys/devices/system/cpu"
        file = "cpufreq/scaling_cur_freq"
        pattern = "cpu"
        divisor = 1e6 :: Double
        failureMessage = "CpuFreq: N/A"
        fmt x | x < 1     = (show (round (x * 1000) :: Integer)) ++ "MHz"
              | otherwise = (show x) ++ "GHz"
    checkedDataRetrieval failureMessage dir file pattern (/divisor) fmt

