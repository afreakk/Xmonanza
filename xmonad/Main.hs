import XMonad as XM
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.StackSet as SS
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.WorkspaceNames
import XMonad.Prompt
import XMonad.Layout.WorkspaceDir
import XMonad.Actions.CopyWindow
import XMonad.Layout.SubLayouts
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

import AConfig (getConfig, AConfig (..))
import XmobarUtils (xmobarShorten)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Calculator (calculatorPrompt)
import BackAndForth (backAndForth)
import ExtraKeyCodes
import LayoutHook (myLayout)

-- Prompt theme
myXPConfig :: AConfig -> XPConfig
myXPConfig cfg = def
    { font                = cl_font cfg
    , bgColor             = cl_black cfg
    , fgColor             = cl_grey cfg
    , bgHLight            = cl_black cfg
    , fgHLight            = cl_lilly cfg
    , borderColor         = cl_lilly cfg
    , promptBorderWidth   = 1
    , height              = fromIntegral $ cl_barHeight cfg
    , position            = Bottom
    , historySize         = 100
    , historyFilter       = deleteConsecutive
    , autoComplete        = Nothing
    , completionKey       = (0,xK_Tab)
    }

cmdBrightness arg = "brightnessctl set " ++ arg
cmdSetVolume arg = "~/bin/setSinkVolumeDefault.sh " ++ arg
cmdMaimSelect out = "maim --select --hidecursor --format png " ++ out
cmdPipeImgToClip = " | xclip -selection clipboard -t image/png -i"
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys cfg conf@(XConfig {XM.modMask = modm}) = M.fromList $
    [ ((modm.|.shiftMask,xK_Return), spawn $ XM.terminal conf)
    , ((0,            xK_XF86AudioRaiseVolume  ), spawn $ cmdSetVolume "+5%")
    , ((0,            xK_XF86AudioLowerVolume  ), spawn $ cmdSetVolume "-5%")
    , ((0,            xK_XF86MonBrightnessDown ), spawn $ cmdBrightness "5%-")
    , ((0,            xK_XF86MonBrightnessUp   ), spawn $ cmdBrightness "+5%")
    , ((shiftMask,      xK_Print ), spawn $ cmdMaimSelect "/dev/stdout" ++ cmdPipeImgToClip ++ "&& xclip -selection clipboard -t image/png -o | feh -")
    , ((0,              xK_Print ), spawn $ cmdMaimSelect "/dev/stdout" ++ cmdPipeImgToClip)
    , ((modm,           xK_Print ), spawn $ cmdMaimSelect "~/img.png")

    , ((modm .|. shiftMask, xK_k ), spawn "dunstctl history-pop")
    , ((modm              , xK_k ), spawn "dunstctl context")
    , ((modm              , xK_v ), spawn "dunstctl close")
    , ((modm .|. shiftMask, xK_v ), spawn "dunstctl close-all")
    -- , ((modm,               xK_o ), spawn "xmodmap ~/.Xmodmap")
    , ((modm,               xK_p ), spawn "clipmenu")
    , ((modm,               xK_s ), spawn "~/bin/openTerminalWithCurrentPwd.sh")
    , ((modm,               xK_f ), spawn "~/bin/windowselector.sh")
    , ((modm,               xK_y ), spawn "~/bin/terminal.sh")
    , ((modm,               xK_w ), spawn "~/bin/runner.sh")
    , ((modm,               xK_r ), renameWorkspace (myXPConfig cfg))
    , ((modm,               xK_q ), kill1)
    , ((modm,               xK_d ), sendMessage NextLayout)
    -- , ((modm,               xK_g ), goToSelected def)
    , ((modm,               xK_g ), spawn "clipmenu")
    , ((modm,               xK_a ), calculatorPrompt (myXPConfig cfg) )
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_d ), setLayout $ XM.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm .|. shiftMask, xK_t     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_n     ), windows W.focusDown)
    , ((modm,               xK_e     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_t     ), promote)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_n     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_e     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_i     ), sendMessage Expand)
    , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
    , ((modm .|. controlMask, xK_n), sendMessage $ pullGroup D)
    , ((modm .|. controlMask, xK_e), sendMessage $ pullGroup U)
    , ((modm .|. controlMask, xK_i), sendMessage $ pullGroup R)

    , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    , ((modm .|. controlMask, xK_x), withFocused (sendMessage . UnMerge))

    , ((modm .|. controlMask, xK_period), onGroup W.focusUp')
    , ((modm .|. controlMask, xK_comma), onGroup W.focusDown')
    -- Push window back into tiling (from float)
    , ((modm,               xK_x     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_r     ), resetWorkspaceNames)
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_grave     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm              , xK_grave     ), spawn "xmonad-afreak --recompile; xmonad-afreak --restart")
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_h ), spawn ("grep 'xK_' ~/coding/Xmonanza/xmonad/Main.hs | dmenu -l 42"))
    , ((modm, xK_c     ), changeDir (myXPConfig cfg))
    , ((modm, xK_z), withFocused $ windows . (`W.float` (W.RationalRect 0 0 1 1)))
    ]
    ++
    [((m .|. modm, k), f i)
      | (i, k) <- zip workspaceNames workspaceKeys
      , (f, m) <- [ (backAndForth, 0)
                  , (windows . W.shift, shiftMask)
                  , (swapWithCurrent,   controlMask)
                  , (windows . copy,    mod1Mask)
                  ]
    ]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_l, xK_u] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

resetWorkspaceNames :: X ()
resetWorkspaceNames = sequence_ $ map (`setWorkspaceName` "") workspaceNames

workspaceNames :: [String]
workspaceNames = map show $ [1..9 :: Int] ++ [0]

workspaceKeys :: [KeySym]
workspaceKeys = [xK_1..xK_9] ++ [xK_0]

promote :: X ()
promote = windows $ SS.modify' $
             \c -> case c of
                   SS.Stack _ [] []     -> c
                   SS.Stack t [] (x:rs) -> SS.Stack x [] (t:rs)
                   SS.Stack t ls rs     -> SS.Stack t [] (reverse ls ++ rs)

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XM.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> XM.focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> XM.focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> XM.focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


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
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook = composeAll [ className =? "qutebrowser" --> unfloat , className =? "TeamViewer" --> unfloat ]
    where unfloat = ask >>= doF . W.sink
-- myManageHook = composeAll
    -- [ className =? "MPlayer"        --> doFloat
    -- , className =? "Gimp"           --> doFloat
    -- , resource  =? "desktop_window" --> doIgnore
    -- , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook <+> fullscreenEventHook

clickableWs wsName = xmobarAction ("xdotool key Super_L+" ++ wsIdx) "1" wsName
  where wsIdx = takeWhile (/=':') $ xmobarStrip wsName

xmobarTitleAllowedChars = [' '..'~']
------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook xmproc cfg = do
  workspaceHistoryHook
  workspaceNamesPP def
    { ppOutput  = hPutStrLn xmproc . xmobarShorten 64
    , ppCurrent = xmobarColor (cl_lilly cfg) ""
    , ppHidden  = clickableWs
    , ppTitle   = xmobarColor (cl_lilly cfg) ""
    , ppTitleSanitize = Prelude.filter (`elem` xmobarTitleAllowedChars) . xmobarStrip
    , ppUrgent  = xmobarColor (cl_aqua  cfg) "" . clickableWs
    , ppOrder   = \(wsNames:layoutName:windowTitle:_) -> [wsNames,windowTitle]
    , ppSep     = " | "
    , ppVisible = xmobarColor (cl_green cfg) ""
    } >>= dynamicLogWithPP

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

main = do
  xmobarproc <- spawnPipe "~/.local/bin/xmobar-afreak"
  cfg <- getConfig
  let xx = defaults xmobarproc cfg
  xmonad . ewmh . ( withUrgencyHook NoUrgencyHook ) . docks $ xx

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
defaults xmobarproc cfg = def {
      -- simple stuff
        terminal           = "alacritty",
        focusFollowsMouse  = False,
      -- Whether clicking on a window to focus also passes the click to the window
        clickJustFocuses   = False,
      -- Width of the window border in pixels.
        borderWidth        = 5,
        modMask            = mod4Mask,
      -- The default number of workspaces (virtual screens) and their names.
      -- By default we use numeric strings, but any string may be used as a
      -- workspace name. The number of workspaces is determined by the length
      -- of this list.
      --
      -- A tagging example:
      --
      -- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
        XM.workspaces      = workspaceNames,
        normalBorderColor  = cl_black cfg,
        focusedBorderColor = cl_grey cfg,
      -- key bindings
        keys               = myKeys cfg,
        mouseBindings      = myMouseBindings,
      -- hooks, layouts
        layoutHook         = smartBorders . avoidStruts . ( workspaceDir "/home/afreak/") $ myLayout (myTabConfig cfg),
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook xmobarproc cfg,
        startupHook        = myStartupHook
    }

