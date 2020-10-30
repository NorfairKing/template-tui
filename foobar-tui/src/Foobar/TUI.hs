{-# LANGUAGE OverloadedStrings #-}

module Foobar.TUI where

import Brick.Main
import Foobar.TUI.Draw
import Foobar.TUI.Handle
import Foobar.TUI.State

foobarTUI :: IO ()
foobarTUI = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

tuiApp :: App State e ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = pure,
      appAttrMap = buildAttrMap
    }

buildInitialState :: IO State
buildInitialState = pure State
