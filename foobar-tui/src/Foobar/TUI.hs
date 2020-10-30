{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Foobar.TUI where

import Brick.BChan
import Brick.Main
import Control.Concurrent.Async
import Control.Monad.Reader
import Foobar.TUI.Draw
import Foobar.TUI.Env
import Foobar.TUI.Handle
import Foobar.TUI.OptParse
import Foobar.TUI.State
import Foobar.TUI.Worker
import Graphics.Vty (defaultConfig, mkVty)

foobarTUI :: IO ()
foobarTUI = do
  Settings {..} <- getSettings
  initialState <- buildInitialState
  reqChan <- newBChan 1000
  respChan <- newBChan 1000
  let vtyBuilder = mkVty defaultConfig
  firstVty <- vtyBuilder
  let runTui = customMain firstVty vtyBuilder (Just respChan) (tuiApp reqChan) initialState
  let env = Env
  let runWorker = runReaderT (tuiWorker reqChan respChan) env
  -- Left always works because the worker runs forever
  Left endState <- race runTui runWorker
  print endState

tuiApp :: BChan Request -> App State Response ResourceName
tuiApp chan =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent chan,
      appStartEvent = pure,
      appAttrMap = buildAttrMap
    }

buildInitialState :: IO State
buildInitialState = pure State
