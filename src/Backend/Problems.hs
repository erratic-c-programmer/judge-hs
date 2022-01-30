{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.Problems where

import Backend.AppDB
import Control.Exception (SomeException, catch)
import Control.Monad (liftM2, zipWithM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.List.Extra (groupBy, sortOn, stripSuffix)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.Persist
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)

-- | Alias for a tuple representing a testcase: (input, output).
type Testcase = (T.Text, T.Text)

-- | Registers the problem specified by the directory name in the database.
--   The specified directory should have 2 files ("metadata" and "desc"), and one directory ("tcs").
--
--   The file "metadata" should have exactly 3 lines. First line is the title; second line is a list
--   of tags, seperated by commas; and the third is the time limit in milliseconds.
--
--   The file "desc" should contain the problem description, in HTML.
--
--   Finally, the directory "tcs" should be structured as described [here](#tcdirfmt).
--
--   If any one of the above conditions is not met, a Nothing is returned; else, a 'Problem' is
--   returned.
mkProblem :: FilePath -> IO (Maybe Problem)
mkProblem dirName = do
  cond <- doesFileExist (dirName ++ "/metadata") &&& doesFileExist (dirName ++ "/desc") &&& doesDirectoryExist (dirName ++ "tcs")
  if cond
    then do
      [title, tlimit, tags] <- T.splitOn "\n" <$> TIO.readFile ("dirName/" ++ "metadata")
      return $ Just (Problem title dirName Nothing tags (read $ T.unpack tlimit))
    else return Nothing
  where
    (&&&) = liftM2 (&&)

-- | Parses a directory of testcases into a @[[`Testcase`]]@. Takes a path to a directory and
--   returns a list of subtasks, each containing seperate testcases.
--
--   #tcdirfmt#
--   Testcase directories should have @\<subtask\>.\<testcase\>.{in,out}@, where @subtask@ and
--   @testcase@ are natural numbers. All input\/output files without corresponding output\/input
--   files will be ignored, as well as all other files in different formats in the filename.
readTCDir :: String -> IO [[Testcase]]
readTCDir dirName = do
  tcfs <- catch (listDirectory dirName) (\(_ :: SomeException) -> return [])
  -- not the most efficient (amortised quadratic), but it doesn't really matter
  let nosufs =
        let xs = mapMaybe (stripSuffix ".in") tcfs
         in filter (\x -> x ++ ".out" `elem` tcfs) xs
  let ins = map (\x -> dirName ++ "/" ++ x ++ ".in") nosufs
  let outs = map (\x -> dirName ++ "/" ++ x ++ ".out") nosufs

  map (map snd) . groupBy getSubTask . sortOn fst . zip ins <$> zipWithM ftoTC ins outs
  where
    ftoTC i o = (,) <$> TIO.readFile i <*> TIO.readFile o
    getSubTask (s1, _) (s2, _) = takeWhile (/= '.') s1 == takeWhile (/= '.') s2
