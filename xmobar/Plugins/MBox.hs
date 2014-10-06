-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.MBox
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for checking mail in mbox files.
--
-----------------------------------------------------------------------------

module Plugins.MBox (MBox(..)) where

import Prelude hiding (catch)
import Plugins

import Control.Monad
import Control.Concurrent.STM
import Control.Exception (SomeException, handle, evaluate)

import System.Directory
import System.FilePath
import System.Console.GetOpt
import System.INotify


import qualified Data.ByteString.Lazy.Char8 as B

data Options = Options
               { oAll :: Bool
               , oUniq :: Bool
               , oDir :: FilePath
               , oPrefix :: String
               , oSuffix :: String
               }

defaults :: Options
defaults = Options {
  oAll = False, oUniq = False, oDir = "", oPrefix = "", oSuffix = ""
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option "a" ["all"] (NoArg (\o -> o { oAll = True })) ""
  , Option "u" [] (NoArg (\o -> o { oUniq = True })) ""
  , Option "d" ["dir"] (ReqArg (\x o -> o { oDir = x }) "") ""
  , Option "p" ["prefix"] (ReqArg (\x o -> o { oPrefix = x }) "") ""
  , Option "s" ["suffix"] (ReqArg (\x o -> o { oSuffix = x }) "") ""
  ]

parseOptions :: [String] -> IO Options
parseOptions args =
  case getOpt Permute options args of
    (o, _, []) -> return $ foldr id defaults o
    (_, _, errs) -> ioError . userError $ concat errs

-- | A list of display names, paths to mbox files and display colours,
-- followed by a list of options.
data MBox = MBox [(String, FilePath, String)] [String] String
          deriving (Read, Show)

instance Exec MBox where
  alias (MBox _ _ a) = a
  start (MBox ms args _) cb = do
    vs <- mapM (const $ newTVarIO ("", 0 :: Int)) ms

    opts <- parseOptions args -- $ words args
    let dir = oDir opts
        allb = oAll opts
        pref = oPrefix opts
        suff = oSuffix opts
        uniq = oUniq opts

    dirExists <- doesDirectoryExist dir
    let ts = map (\(t, _, _) -> t) ms
        sec = \(_, f, _) -> f
        md = if dirExists then (dir </>) . sec else sec
        fs = map md ms
        cs = map (\(_, _, c) -> c) ms
        ev = [CloseWrite]

    i <- initINotify
    zipWithM_ (\f v -> addWatch i ev f (handleNotification v)) fs vs

    forM_ (zip fs vs) $ \(f, v) -> do
      exists <- doesFileExist f
      n <- if exists then countMails f else return 0
      atomically $ writeTVar v (f, n)

    changeLoop (mapM (fmap snd . readTVar) vs) $ \ns ->
      let s = unwords [ showC uniq m n c | (m, n, c) <- zip3 ts ns cs
                                         , allb || n /= 0 ]
      in cb (if length s == 0 then "" else pref ++ s ++ suff)

showC :: Bool -> String -> Int -> String -> String
showC u m n c =
  if c == "" then msg else "<fc=" ++ c ++ ">" ++ msg ++ "</fc>"
    where msg = m ++ if not u || n > 1 then show n else ""

countMails :: FilePath -> IO Int
countMails f =
  handle ((\_ -> evaluate 0) :: SomeException -> IO Int)
         (do txt <- B.readFile f
             evaluate $! length . filter (B.isPrefixOf from) . B.lines $ txt)
  where from = B.pack "From "

handleNotification :: TVar (FilePath, Int) -> Event -> IO ()
handleNotification v _ =  do
  (p, _) <- atomically $ readTVar v
  n <- countMails p
  atomically $ writeTVar v (p, n)

changeLoop :: Eq a => STM a -> (a -> IO ()) -> IO ()
changeLoop s f = atomically s >>= go
 where
    go old = do
        f old
        go =<< atomically (do
            new <- s
            guard (new /= old)
            return new)
