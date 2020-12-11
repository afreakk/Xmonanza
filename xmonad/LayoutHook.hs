{-# LANGUAGE FlexibleContexts #-}
module LayoutHook (myLayout) where
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.Simplest
import XMonad.Layout.Reflect
import XMonad.Layout.WindowNavigation
import XMonad.Hooks.ManageDocks as MD
import XMonad.Layout
import AConfig (AConfig (..))
import BooleanLayout
import XMonad.Layout.BoringWindows as BRNG

myTabConfig :: AConfig -> XMonad.Layout.Tabbed.Theme
myTabConfig cfg = def
  { activeTextColor = cl_black cfg 
  , inactiveTextColor = cl_lilly cfg 
  , activeColor = cl_lilly cfg 
  , activeBorderColor = cl_lilly cfg 
  , inactiveColor = cl_black cfg 
  , inactiveBorderColor = cl_lilly cfg 
  , urgentColor = cl_aqua cfg 
  , urgentBorderColor = cl_lilly cfg 
  , fontName = cl_font cfg
  , decoHeight = fromIntegral $ cl_barHeight cfg
  }

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
myLayout cfg = smartBorders $ MD.avoidStruts $ BRNG.boringWindows $ dynamicTiledSubTabbed ||| tabbed
  where
    tabcfg                = myTabConfig cfg
    tabbed                = tabbedBottom shrinkText tabcfg

    dynamicTiledSubTabbed = configurableNavigation noNavigateBorders $ subTabbedBottom tabcfg dynamicTiled
    dynamicTiled          = BooleanLayout isHogwarts hogwartsTiled tiled
    hogwartsTiled         = reflectHoriz $ reflectVert $ Mirror tiled
    tiled                 = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = toRational (2/(1 + sqrt 5 :: Double))
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
    subTabbedBottom tabcfg parentLayout = addTabsBottom shrinkText tabcfg $ subLayout [] Simplest parentLayout
    isHogwarts = cl_hostName cfg == "hogwarts"
