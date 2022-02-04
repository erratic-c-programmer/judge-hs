{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Conduit
import Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Lib
import Yesod
import Yesod.Default.Util

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
  / HomeR GET
  /echo/#T.Text EchoR GET
  /problems ProblemsR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello world!|]

getEchoR :: T.Text -> Handler Html
getEchoR text = defaultLayout [whamlet|<h1>#{text}|]

getProblemsR :: Handler Html
getProblemsR = undefined

fetchProblems :: SqlPersistT IO [Entity Problem]
fetchProblems = selectList [] []

main :: IO ()
main = warp 3000 HelloWorld
