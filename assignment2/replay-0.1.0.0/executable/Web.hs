module Web
  (
    Web

  , io
  , ask
  , runWeb
  ) where

import Web.Scotty (ActionM)

import Replay

type Web a = Replay Form Answers a

data Form

data Answers

runWeb :: Web () -> ActionM ()
runWeb = undefined
