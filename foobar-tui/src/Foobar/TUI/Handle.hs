module Foobar.TUI.Handle where

import Brick.Main
import Brick.Types
import Foobar.TUI.State
import Graphics.Vty.Input.Events

handleTuiEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleTuiEvent s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s
    _ -> continue s
