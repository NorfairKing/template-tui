module Foobar.TUI.State where

data State = State
  deriving (Show)

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)
