{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Plugins.Monitors
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The system monitor plugin for Xmobar.
--
-----------------------------------------------------------------------------

module Plugins.Monitors where

import Plugins

import Plugins.Monitors.Common ( runM )
import Plugins.Monitors.Weather
import Plugins.Monitors.Net
import Plugins.Monitors.Mem
import Plugins.Monitors.Swap
import Plugins.Monitors.Cpu
import Plugins.Monitors.MultiCpu
import Plugins.Monitors.Batt
import Plugins.Monitors.Thermal
import Plugins.Monitors.CpuFreq
import Plugins.Monitors.CoreTemp
import Plugins.Monitors.Disk
import Plugins.Monitors.Top
#ifdef IWLIB
import Plugins.Monitors.Wireless
#endif
#ifdef LIBMPD
import Plugins.Monitors.MPD
#endif

data Monitors = Weather  Station    Args Rate
              | Network  Interface  Args Rate
              | Memory   Args       Rate
              | Swap     Args       Rate
              | Cpu      Args       Rate
              | MultiCpu Args       Rate
              | Battery  Args       Rate
              | BatteryP [String]   Args Rate
              | DiskU    DiskSpec   Args Rate
              | DiskIO   DiskSpec   Args Rate
              | Thermal  Zone       Args Rate
              | CpuFreq  Args       Rate
              | CoreTemp Args       Rate
              | TopProc  Args       Rate
              | TopMem   Args       Rate
#ifdef IWLIB
              | Wireless Interface  Args Rate
#endif
#ifdef LIBMPD
              | MPD      Args       Rate
#endif
                deriving (Show,Read,Eq)

type Args      = [String]
type Program   = String
type Alias     = String
type Station   = String
type Zone      = String
type Interface = String
type Rate      = Int
type DiskSpec  = [(String, String)]

instance Exec Monitors where
    alias (Weather  s _ _) = s
    alias (Network  i _ _) = i
    alias (Thermal  z _ _) = z
    alias (Memory     _ _) = "memory"
    alias (Swap       _ _) = "swap"
    alias (Cpu        _ _) = "cpu"
    alias (MultiCpu   _ _) = "multicpu"
    alias (Battery    _ _) = "battery"
    alias (BatteryP  _ _ _)= "battery"
    alias (CpuFreq    _ _) = "cpufreq"
    alias (TopProc    _ _) = "top"
    alias (TopMem     _ _) = "topmem"
    alias (CoreTemp   _ _) = "coretemp"
    alias (DiskU    _ _ _) = "disku"
    alias (DiskIO   _ _ _) = "diskio"
#ifdef IWLIB
    alias (Wireless i _ _) = i ++ "wi"
#endif
#ifdef LIBMPD
    alias (MPD        _ _) = "mpd"
#endif
    start (Weather  s a r) = runM (a ++ [s]) weatherConfig  runWeather    r
    start (Network  i a r) = runM (a ++ [i]) netConfig      runNet        r
    start (Thermal  z a r) = runM (a ++ [z]) thermalConfig  runThermal    r
    start (Memory     a r) = runM a          memConfig      runMem        r
    start (Swap       a r) = runM a          swapConfig     runSwap       r
    start (Cpu        a r) = runM a          cpuConfig      runCpu        r
    start (MultiCpu   a r) = runM a          multiCpuConfig runMultiCpu   r
    start (Battery    a r) = runM a          battConfig     runBatt       r
    start (BatteryP s a r) = runM a          battConfig    (runBatt' s)   r
    start (CpuFreq    a r) = runM a          cpuFreqConfig  runCpuFreq    r
    start (CoreTemp   a r) = runM a          coreTempConfig runCoreTemp   r
    start (DiskU    s a r) = runM a          diskUConfig   (runDiskU s)   r
    start (DiskIO   s a r) = runM a          diskIOConfig  (runDiskIO s)  r
    start (TopMem     a r) = runM a          topMemConfig   runTopMem     r
    start (TopProc    a r) = startTop a r
#ifdef IWLIB
    start (Wireless i a r) = runM (a ++ [i]) wirelessConfig runWireless   r
#endif
#ifdef LIBMPD
    start (MPD        a r) = runM a          mpdConfig      runMPD        r
#endif
