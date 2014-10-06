-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.CoreCommon
-- Copyright   :  (c) Juraj Hercek
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Juraj Hercek <juhe_haskell@hck.sk>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The common part for cpu core monitors (e.g. cpufreq, coretemp)
--
-----------------------------------------------------------------------------

module Plugins.Monitors.CoreCommon where

import Plugins.Monitors.Common
import System.Posix.Files (fileExist)
import System.IO (withFile, IOMode(ReadMode), hGetLine)
import System.Directory
import Data.Char (isDigit)
import Data.List (isPrefixOf)

-- |
-- Function checks the existence of first file specified by pattern and if the
-- file doesn't exists failure message is shown, otherwise the data retrieval
-- is performed.
checkedDataRetrieval :: (Num a, Ord a, Show a) =>
                        String -> String -> String -> String -> (Double -> a)
                        -> (a -> String) -> Monitor String
checkedDataRetrieval failureMessage dir file pattern trans fmt = do
    exists <- io $ fileExist $ concat [dir, "/", pattern, "0/", file]
    case exists of
         False  -> return failureMessage
         True   -> retrieveData dir file pattern trans fmt

-- |
-- Function retrieves data from files in directory dir specified by
-- pattern. String values are converted to double and 'trans' applied
-- to each one. Final array is processed by template parser function
-- and returned as monitor string.
retrieveData :: (Num a, Ord a, Show a) =>
                String -> String -> String -> (Double -> a) -> (a -> String) ->
                Monitor String
retrieveData dir file pattern trans fmt = do
    count <- io $ dirCount dir pattern
    contents <- io $ mapM getGuts $ files count
    values <- mapM (showWithColors fmt) $ map conversion contents
    parseTemplate values
    where
        getGuts f = withFile f ReadMode hGetLine
        dirCount path str = getDirectoryContents path
                            >>= return . length
                                       . filter (\s -> str `isPrefixOf` s
                                                       && isDigit (last s))
        files count = map (\i -> concat [dir, "/", pattern, show i, "/", file])
                          [0 .. count - 1]
        conversion = trans . (read :: String -> Double)

