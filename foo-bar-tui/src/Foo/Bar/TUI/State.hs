module Foo.Bar.TUI.State where

data State = State
  deriving (Show)

data ResourceName = ResourceName
  deriving (Show, Eq, Ord)
