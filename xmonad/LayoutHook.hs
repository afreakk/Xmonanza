{-# LANGUAGE FlexibleContexts #-}
module LayoutHook (myLayout) where
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.Simplest
import XMonad as XM
import XMonad.Layout.Reflect
------------------------------------------------------------------------
-- Layouts:
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout tabcfg = bigScreenBottomTiled ||| tabbed
  where
    bigScreenBottomTiled = (addTabsBottom shrinkText tabcfg (subLayout [] Simplest (reflectHoriz (reflectVert (Mirror tiled)))))
    tabbed      = tabbedBottom shrinkText tabcfg
    -- normalTiled = (addTabsBottom shrinkText tabcfg (subLayout [] Simplest tiled))
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 4/7
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

