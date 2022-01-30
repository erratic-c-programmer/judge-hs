module Lib
  ( module Backend.AppDB,
    module Backend.Submissions
  )
    where

import Backend.AppDB
import Backend.Submissions

someFunc :: IO ()
someFunc = putStrLn "someFunc"
