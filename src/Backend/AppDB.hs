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

module Backend.AppDB where

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
    User
      -- | username
      uname Text
      -- | account creation time; stored as seconds since the epoch
      creationtime Int
      deriving Show

    Problem
      -- | this is displayed
      title Text
      -- | problem directory; should minimally contain an desc and tc/
      dir FilePath
      -- | link to url, if any
      pdfurl Text Maybe
      -- | problem tags; comma seperated
      tags Text
      -- | time limit in microseconds
      tlimit Int
      deriving Show

    Submission
      -- | which problem this is for
      problemid ProblemId
      -- | language the submission is written in
      language Language
      -- | score of the submission, between 0 and 100 inclusive
      score Int
      -- | actual code of the submission
      code Text
      deriving Show

    UserSolves
      -- | id of user who solved problem
      userid UserId
      -- | id of submission
      submissionid SubmissionId
      deriving Show
|]
