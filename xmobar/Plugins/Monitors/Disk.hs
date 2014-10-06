-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Disk
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
--  Disk usage and throughput monitors for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Disk ( diskUConfig, runDiskU
                             , diskIOConfig, runDiskIO
                             ) where

import Plugins.Monitors.Common
import StatFS

import Control.Monad (zipWithM)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (isPrefixOf, find, intercalate)

diskIOConfig :: IO MConfig
diskIOConfig = mkMConfig "" ["total", "read", "write",
                             "totalbar", "readbar", "writebar"]

diskUConfig :: IO MConfig
diskUConfig = mkMConfig ""
              ["size", "free", "used", "freep", "usedp", "freebar", "usedbar"]

type DevName = String
type Path = String

mountedDevices :: [String] -> IO [(DevName, Path)]
mountedDevices req = do
  s <- B.readFile "/etc/mtab"
  return (parse s)
  where
    parse = map undev . filter isDev . map (firstTwo . B.words) . B.lines
    firstTwo (a:b:_) = (B.unpack a, B.unpack b)
    firstTwo _ = ("", "")
    isDev (d, p) = "/dev/" `isPrefixOf` d &&
                   (p `elem` req || drop 5 d `elem` req)
    undev (d, f) = (drop 5 d, f)

diskData :: IO [(DevName, [Float])]
diskData = do
  s <- B.readFile "/proc/diskstats"
  let extract ws = (head ws, map read (tail ws))
  return $ map (extract . map B.unpack . drop 2 . B.words) (B.lines s)

mountedData :: [DevName] -> IO [(DevName, [Float])]
mountedData devs = do
  (dt, dt') <- doActionTwiceWithDelay 750000 diskData
  return $ map (parseDev (zipWith diff dt' dt)) devs
  where diff (dev, xs) (_, ys) = (dev, zipWith (-) xs ys)

parseDev :: [(DevName, [Float])] -> DevName -> (DevName, [Float])
parseDev dat dev =
  case find ((==dev) . fst) dat of
    Nothing -> (dev, [0, 0, 0])
    Just (_, xs) ->
      let rSp = speed (xs !! 2) (xs !! 3)
          wSp = speed (xs !! 6) (xs !! 7)
          sp =  speed (xs !! 2 + xs !! 6) (xs !! 3 + xs !! 7)
          speed x t = if t == 0 then 0 else 500 * x / t
          dat' = if length xs > 6 then [sp, rSp, wSp] else [0, 0, 0]
      in (dev, dat')

fsStats :: String -> IO [Integer]
fsStats path = do
  stats <- getFileSystemStats path
  case stats of
    Nothing -> return [-1, -1, -1]
    Just f -> let tot = fsStatByteCount f
                  free = fsStatBytesAvailable f
                  used = fsStatBytesUsed f
              in return [tot, free, used]

speedToStr :: Float -> String
speedToStr = showWithUnits 2 1

sizeToStr :: Integer -> String
sizeToStr = showWithUnits 3 0 . fromIntegral

findTempl :: DevName -> Path -> [(String, String)] -> String
findTempl dev path disks =
  case find devOrPath disks of
    Just (_, t) -> t
    Nothing -> ""
  where devOrPath (d, _) = d == dev || d == path

devTemplates :: [(String, String)]
                -> [(DevName, Path)]
                -> [(DevName, [Float])]
                -> [(String, [Float])]
devTemplates disks mounted dat =
  map (\(d, p) -> (findTempl d p disks, findData d)) mounted
  where findData dev = case find ((==dev) . fst) dat of
                         Nothing -> [0, 0, 0]
                         Just (_, xs) -> xs

runDiskIO' :: (String, [Float]) -> Monitor String
runDiskIO' (tmp, xs) = do
  s <- mapM (showWithColors speedToStr) xs
  b <- mapM (showLogBar 0.8) xs
  setConfigValue tmp template
  parseTemplate $ s ++ b

runDiskIO :: [(String, String)] -> [String] -> Monitor String
runDiskIO disks _ = do
  mounted <- io $ mountedDevices (map fst disks)
  dat <- io $ mountedData (map fst mounted)
  strs <- mapM runDiskIO' $ devTemplates disks mounted dat
  return $ intercalate " " strs

runDiskU' :: String -> String -> Monitor String
runDiskU' tmp path = do
  setConfigValue tmp template
  fstats <- io $ fsStats path
  let strs = map sizeToStr fstats
      freep = (fstats !! 1) * 100 `div` head fstats
      fr = fromIntegral freep / 100
  s <- zipWithM showWithColors' strs [100, freep, 100 - freep]
  sp <- showPercentsWithColors [fr, 1 - fr]
  fb <- showPercentBar (fromIntegral freep) fr
  ub <- showPercentBar (fromIntegral $ 100 - freep) (1 - fr)
  parseTemplate $ s ++ sp ++ [fb, ub]

runDiskU :: [(String, String)] -> [String] -> Monitor String
runDiskU disks _ = do
  devs <- io $ mountedDevices (map fst disks)
  strs <- mapM (\(d, p) -> runDiskU' (findTempl d p disks) p) devs
  return $ intercalate " " strs
