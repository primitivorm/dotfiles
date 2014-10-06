-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Top
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
--  Process activity and memory consumption monitors
--
-----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Plugins.Monitors.Top (startTop, topMemConfig, runTopMem) where

import Plugins.Monitors.Common
import Plugins.Monitors.Mem (usedMem)

import Control.Exception (SomeException, handle)
import System.Directory
import System.FilePath
import System.IO
import System.Posix.Unistd (getSysVar, SysVar(ClockTick))
import Foreign.C.Types
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.IORef
import Data.Time.Clock

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

intStrs :: [String]
intStrs = map show [(1::Int) ..]

topMemConfig :: IO MConfig
topMemConfig = mkMConfig "<both1>"
                 [ k ++ n | n <- intStrs , k <- ["name", "mem", "both"]]

topConfig :: IO MConfig
topConfig = mkMConfig "<both1>"
              ("no" : [ k ++ n | n <- intStrs
                               , k <- [ "name", "cpu", "both"
                                      , "mname", "mem", "mboth"]])

foreign import ccall "unistd.h getpagesize"
  c_getpagesize :: CInt

pageSize :: Float
pageSize = fromIntegral c_getpagesize / 1024

showInfo :: String -> String -> Float -> Monitor [String]
showInfo nm sms mms = do
  mnw <- getConfigValue maxWidth
  mxw <- getConfigValue minWidth
  let lsms = length sms
      nmw = mnw - lsms - 1
      nmx = mxw - lsms - 1
      rnm = if nmw > 0 then padString nmw nmx " " True nm else nm
  mstr <- showWithColors' sms mms
  both <- showWithColors' (rnm ++ " " ++ sms) mms
  return [nm, mstr, both]

processes :: IO [FilePath]
processes = fmap (filter isPid) (getDirectoryContents "/proc")
  where isPid = all (`elem` ['0'..'9'])

getProcessData :: FilePath -> IO [String]
getProcessData pidf =
  handle (const (return []) :: SomeException -> IO [String])
         (withFile ("/proc" </> pidf </> "stat") ReadMode readWords)
  where readWords = fmap words . hGetLine

handleProcesses :: ([String] -> a) -> IO [a]
handleProcesses f =
  fmap (foldr (\p ps -> if p == [] then ps else f p : ps) [])
       (processes >>= mapM getProcessData)

processName :: [String] -> String
processName = drop 1 . init . (!!1)

sortTop :: [(a, Float)] -> [(a, Float)]
sortTop =  sortBy (flip (comparing snd))

type Meminfo = (String, Float)

meminfo :: [String] -> Meminfo
meminfo fs = (processName fs, pageSize * read (fs!!23))

meminfos :: IO [Meminfo]
meminfos = handleProcesses meminfo

showMeminfo :: Float -> Meminfo -> Monitor [String]
showMeminfo scale (nm, rss) =
  showInfo nm (showWithUnits 2 1 rss) (rss / (1024 * scale))

runTopMem :: [String] -> Monitor String
runTopMem _ = do
  ps <- io meminfos
  pstr <- mapM (showMeminfo 1) $ sortTop ps
  parseTemplate $ concat pstr

type Pid = Int
type TimeInfo = (String, Float)
type TimeEntry = (Pid, TimeInfo)
type Times = IntMap TimeInfo
type TimesRef = IORef (Times, UTCTime)

timeMemEntry :: [String] -> (TimeEntry, Meminfo)
timeMemEntry fs = ((p, (n, t)), (n, r))
  where p = read (head fs)
        n = processName fs
        t = read (fs!!13) + read (fs!!14)
        (_, r) = meminfo fs

timeMemEntries :: IO [(TimeEntry, Meminfo)]
timeMemEntries = handleProcesses timeMemEntry

timeMemInfos :: IO (Times, [Meminfo], Int)
timeMemInfos =
  fmap (\x -> (M.fromList . map fst $ x, map snd x, length x)) timeMemEntries

combineTimeInfos :: Times -> Times -> Times
combineTimeInfos t0 t1 = M.intersectionWith timeDiff t1 t0
  where timeDiff (n, x1) (_, x0) = (n, x1 - x0)

topProcesses :: TimesRef -> Float -> IO (Int, [TimeInfo], [Meminfo])
topProcesses tref scale = do
  (t1, mis, len) <- timeMemInfos
  c1 <- getCurrentTime
  atomicModifyIORef tref $ \(t0, c0) ->
    let scx = (fromRational . toRational $ diffUTCTime c1 c0) * scale / 100
        ts = M.elems $ combineTimeInfos t0 t1
        nts = map (\(nm, t) -> (nm, t / scx)) ts
    in ((t1, c1), (len, sortTop nts, sortTop mis))

showTimeInfo :: TimeInfo -> Monitor [String]
showTimeInfo (n, t) = showInfo n (showDigits 1 t) t

runTop :: TimesRef -> Float -> Float -> [String] -> Monitor String
runTop tref scale mscale _ = do
  (no, ps, ms) <- io $ topProcesses tref scale
  pstr <- mapM showTimeInfo ps
  mstr <- mapM (showMeminfo mscale) ms
  parseTemplate $! show no : concat (zipWith (++) pstr mstr)

startTop :: [String] -> Int -> (String -> IO ()) -> IO ()
startTop a r cb = do
  cr <- getSysVar ClockTick
  m <- usedMem
  c <- getCurrentTime
  tref <- newIORef (M.empty, c)
  runM a topConfig (runTop tref (fromIntegral cr) m) r cb
