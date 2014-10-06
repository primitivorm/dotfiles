-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Mail
-- Copyright   :  (c) Spencer Janssen
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <sjanssen@cse.unl.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for checking mail.
--
-----------------------------------------------------------------------------

module Plugins.Mail where

import Prelude hiding (catch)
import Plugins

import Control.Monad
import Control.Concurrent.STM

import System.Directory
import System.FilePath
import System.INotify

import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as S

-- | A list of mail box names and paths to maildirs.
data Mail = Mail [(String, FilePath)]
    deriving (Read, Show)

instance Exec Mail where
    start (Mail ms) cb = do
        vs <- mapM (const $ newTVarIO S.empty) ms

        let ts = map fst ms
            ds = map ((</> "new") . snd) ms
            ev = [Move, MoveIn, MoveOut, Create, Delete]

        i <- initINotify
        zipWithM_ (\d v -> addWatch i ev d (handle v)) ds vs

        forM (zip ds vs) $ \(d, v) -> do
            s <- fmap (S.fromList . filter (not . isPrefixOf "."))
                    $ getDirectoryContents d
            atomically $ modifyTVar v (S.union s)

        changeLoop (mapM (fmap S.size . readTVar) vs) $ \ns -> do
            cb . unwords $ [m ++ ":" ++  show n
                                    | (m, n) <- zip ts ns
                                    , n /= 0 ]

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar v f = readTVar v >>= writeTVar v . f

handle :: TVar (Set String) -> Event -> IO ()
handle v e = atomically $ modifyTVar v $ case e of
    Created  {} -> create
    MovedIn  {} -> create
    Deleted  {} -> delete
    MovedOut {} -> delete
    _           -> id
 where
    delete = S.delete (filePath e)
    create = S.insert (filePath e)

changeLoop :: Eq a => STM a -> (a -> IO ()) -> IO ()
changeLoop s f = atomically s >>= go
 where
    go old = do
        f old
        go =<< atomically (do
            new <- s
            guard (new /= old)
            return new)
