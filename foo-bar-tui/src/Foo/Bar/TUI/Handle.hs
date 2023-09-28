module Foo.Bar.TUI.Handle where

import Brick.BChan
import Brick.Main
import Brick.Types
import Control.Monad.IO.Class
import Foo.Bar.TUI.Env
import Foo.Bar.TUI.State
import Graphics.Vty.Input.Events

handleTuiEvent :: BChan Request -> BrickEvent n Response -> EventM n State ()
handleTuiEvent chan e =
  case e of
    VtyEvent vtye ->
      case vtye of
        EvKey (KChar 'q') [] -> halt
        EvKey (KChar 'e') [] -> liftIO $ writeBChan chan Request
        _ -> pure ()
    AppEvent resp -> case resp of
      Response -> pure ()
    _ -> pure ()
