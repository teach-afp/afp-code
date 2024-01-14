{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty (scotty, get, post)

import Web

main :: IO ()
main = scotty 3000 $ do
  get  "/" (runWeb mainWeb)
  post "/" (runWeb mainWeb)

-- a realistic example that uses 'Web'
mainWeb :: Web ()
mainWeb = undefined
