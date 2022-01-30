{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Lib
import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
  / HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello world!|]

main :: IO ()
main = warp 3000 HelloWorld
