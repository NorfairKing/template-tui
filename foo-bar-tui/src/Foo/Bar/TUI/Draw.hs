module Foo.Bar.TUI.Draw where

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Foo.Bar.TUI.State
import Graphics.Vty.Attributes

buildAttrMap :: State -> AttrMap
buildAttrMap = const $ attrMap defAttr []

drawTui :: State -> [Widget ResourceName]
drawTui s = [str $ show s]
