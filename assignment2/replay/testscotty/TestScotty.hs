{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Catch (SomeException, catch)
import Data.Text.Lazy      (Text)

import Web.Scotty
  ( ActionM, scotty, middleware, get, post, html, formParam )

import Network.Wai.Middleware.RequestLogger
  ( mkRequestLogger, defaultRequestLoggerSettings )

main :: IO ()
main = do
  logger <- mkRequestLogger defaultRequestLoggerSettings
  scotty 3000 do
    middleware logger
    get  "/" serve  -- Initial entrypoint
    post "/" serve  -- Entry point for form submission with method=post
  where
    serve :: ActionM ()
    serve = do
      i <- getInput
      html (page i)

    getInput :: ActionM Text
    getInput = formParam "text_input_id" `catch` \ (_ :: SomeException) -> return ""

    page :: Text -> Text
    page s = mconcat $
      [ "<html><body>"
      , "<p>Input was: ", s, "</p>"
      , "<form method=post>"
      , "<p>Type something here:</p>"
      , "<input name=text_input_id>"
      , "<input type=submit value=OK>"
      , "</form>"
      , "</body></html>"
      ]
