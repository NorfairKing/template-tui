module Foo.Bar.TUI.Handle where

import Brick.BChan
import Brick.Main
import Brick.Types
import Control.Monad.IO.Class
import Foo.Bar.TUI.Env
import Foo.Bar.TUI.State
import Graphics.Vty.Input.Events

handleTuiEvent :: BChan Request -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent chan s e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt s
        EvKey (KChar 'e') [] -> do
          liftIO $ writeBChan chan Request
          continue s
        _ -> continue s
    AppEvent resp -> case resp of
      Response -> continue s
    _ -> continue s
