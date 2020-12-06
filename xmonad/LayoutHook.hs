{-# LANGUAGE FlexibleContexts #-}
module LayoutHook (myLayout) where
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.Simplest
import XMonad as XM
import XMonad.Layout.Reflect
import XMonad.Layout.WindowNavigation
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
myLayout tabcfg = bigScreenTiledSubTabbed ||| tabbed
  where
    bigScreenTiledSubTabbed = windowNavigation $ subTabbedBottom tabcfg bigScreenTiled
    tabbed                  = tabbedBottom shrinkText tabcfg
    bigScreenTiled          = reflectHoriz (reflectVert (Mirror tiled))
    -- normalTiled = (addTabsBottom shrinkText tabcfg (subLayout [] Simplest tiled))
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = toRational (2/(1 + sqrt 5 :: Double))
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    subTabbedBottom tabcfg parentLayout = addTabsBottom shrinkText tabcfg $ subLayout [] Simplest parentLayout

