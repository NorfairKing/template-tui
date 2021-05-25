module Foo.Bar.TUI.Env where

import Control.Monad.Reader

data Env = Env
  {
  }

type W = ReaderT Env IO

data Request = Request

data Response = Response
