{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty     (ActionM, scotty, get, post, rescue, html, param)
import Data.Text.Lazy (Text)

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = do
        i <- getInput
        html (page i)

    getInput :: ActionM Text
    getInput = param "text_input_id" `rescue` \ _ -> return ""

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
