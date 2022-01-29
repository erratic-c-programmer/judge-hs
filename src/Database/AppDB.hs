{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.AppDB where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

data Language = Scheme | Python | C | Cpp | Haskell
  deriving (Show, Read, Eq)

derivePersistField "Language"

share
  [mkPersist sqlSettings, mkMigrate "migrateTables"]
  [persistLowerCase|
    Problem
      -- | this is displayed
      title Text
      -- | link to url, if any
      pdfurl Text Maybe
      -- | time limit in microseconds
      tlimit Int
      deriving Show
    Submission
      -- | which problem this is for
      problem ProblemId
      -- | language the submission is written in
      language Language
      -- | actual code of the submission
      code Text
      deriving Show
|]
