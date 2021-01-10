import XMonad as XM
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Layout.Tabbed
import XMonad.Hooks.EwmhDesktops
import XMonad.StackSet as SS
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.WorkspaceNames
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.Man
import XMonad.Actions.CopyWindow
import XMonad.Layout.SubLayouts
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Hooks.FloatNext
import XMonad.Actions.CycleWS
import AConfig (getConfig, AConfig (..), ifHnsTop)
import XmobarUtils (xmobarShorten)
import XMonad.Hooks.ManageDocks as MD
import XMonad.Layout.BoringWindows as BRNG
import XMonad.Layout.ResizableTile
import XMonad.Hooks.RefocusLast

import XMonad.Hooks.ScreenCorners
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Calculator (calculatorPrompt)
import BackAndForth (backAndForth)
import ExtraKeyCodes
import LayoutHook (myLayout)
import XMonad.Actions.Submap
import GridSelects (gsWithWindows, gsWindowGoto, gsActionRunner)
import NamedScratchpadRefocusLast

scratchpads =
    [ NS "spotify" "spotifywm" (className =? "Spotify") (customFloating $ W.RationalRect 0.5 0.01 0.5 0.98)
    , NS "todo" "namedVim.sh todo ~/Dropbox/todo/todo.txt" (wmName =? "todo") (customFloating $ W.RationalRect (1/6) (1/2) (2/3) (1/3))
    , NS "kmag" "kmag" (className =? "kmag") (customFloating $ W.RationalRect 0.05 0.9 0.9 0.1)
    ] where wmName = stringProperty "WM_NAME"

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

myCmds cfg conf =
    [ ("default-layout"      , setLayout $ XM.layoutHook conf)
    , ("recompile"           , spawn "xmonad-afreak --recompile; xmonad-afreak --restart;")
    , ("kill"                , kill1)
    , ("refresh"             , refresh)
    , ("quit-wm"             , io $ exitWith ExitSuccess)
    , ("hotkeys"             , spawn ("grep 'xK_' ~/coding/Xmonanza/xmonad/Main.hs | dmenu -l 42"))
    , ("dunstctl-history-pop", spawn "dunstctl history-pop")
    , ("dunstctl-context"    , spawn "dunstctl context")
    , ("dunstctl-close"      , spawn "dunstctl close")
    , ("dunstctl-close-all"  , spawn "dunstctl close-all")
    , ("dunstctl-action"     , spawn "dunstctl action")
    , ("clip-to~/img.png"    , spawn $ cmdMaimSelect "~/img.png")
    , ("clip-to-feh"         , spawn $ cmdMaimSelect "/dev/stdout" ++ cmdPipeImgToClip ++ "&& xclip -selection clipboard -t image/png -o | feh -")
    , ("setactivesink"       , spawn "~/bin/setActiveSink")
    , ("manPrompt"           , manPrompt (myXPConfig cfg))
    ]

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
    , ((modm,         xK_XF86MonBrightnessDown ), spawn $ cmdBrightness "1")
    , ((0,            xK_XF86MonBrightnessUp   ), spawn $ cmdBrightness "+5%")
    , ((modm,         xK_XF86MonBrightnessUp   ), spawn $ cmdBrightness "100%")
    , ((0,            xK_Print                 ), spawn $ cmdMaimSelect "/dev/stdout" ++ cmdPipeImgToClip)

    , ((modm,               xK_q   ), kill1)
    , ((modm,               xK_w   ), spawn "~/bin/runner.sh")
    , ((modm,               xK_f   ), spawn "notify-send --urgency=low 'sublayout submap'" >>
        (submap . M.fromList $
            [ ((modm, xK_h), sendMessage $ pullGroup L)
            , ((modm, xK_n), sendMessage $ pullGroup U)
            , ((modm, xK_e), sendMessage $ pullGroup D)
            , ((modm, xK_i), sendMessage $ pullGroup R)
            , ((0,    xK_m), withFocused (sendMessage . MergeAll))
            , ((0,    xK_u), withFocused (sendMessage . UnMerge))
            , ((0,    xK_n), onGroup W.focusDown')
            , ((0,    xK_e), onGroup W.focusUp')
            , ((0,    xK_t), onGroup swapMasterOnStack)
            ]
        )
      )
    , ((modm,               xK_p   ), spawn "clipmenu")
    , ((modm,               xK_g   ), gsWindowGoto cfg)

    , ((modm,               xK_a   ), calculatorPrompt (myXPConfig cfg) )
    , ((modm,               xK_r   ), renameWorkspace (myXPConfig cfg))
    , ((modm .|. shiftMask, xK_r   ), resetWorkspaceNames)
    , ((modm,               xK_s   ), spawn "~/bin/openTerminalWithCurrentPwd.sh")
    , ((modm,               xK_t   ), promote)
    , ((modm,               xK_d   ), sendMessage NextLayout)
    , ((modm,                 xK_z ), withFocused $ windows . (`W.float` (W.RationalRect 0 0 1 1)))
    , ((modm,                 xK_x ), withFocused $ windows . W.sink)
    , ((modm,                 xK_c ), gsActionRunner (myCmds cfg conf) cfg)
    , ((modm              ,   xK_b ), sendMessage MD.ToggleStruts)

    , ((modm,               xK_j     ), spawn "~/bin/setxkbscript")
    , ((modm,               xK_y     ), spawn "~/bin/terminal.sh")
    , ((modm .|. shiftMask, xK_y     ), toggleFloatAllNew >> runLogHook)

    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm .|. shiftMask, xK_h), sendMessage MirrorShrink)
    , ((modm,               xK_n     ), BRNG.focusDown)
    , ((modm .|. shiftMask, xK_n     ), windows W.swapDown  )
    , ((modm,               xK_e     ), BRNG.focusUp  )
    , ((modm .|. shiftMask, xK_e     ), windows W.swapUp    )
    , ((modm,               xK_i     ), sendMessage Expand)
    , ((modm .|. shiftMask, xK_i     ), sendMessage MirrorExpand)
    , ((modm,               xK_o     ), namedScratchpadAction scratchpads "todo")
    , ((modm,               xK_semicolon), namedScratchpadAction scratchpads "spotify")

    , ((modm,               xK_period), windows W.focusMaster  )
    , ((modm              , xK_m     ), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm,               xK_Tab   ), nextWS)
    , ((modm .|. shiftMask, xK_Tab   ), prevWS)
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
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_l, xK_u] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

swapMasterOnStack (W.Stack f u d) = W.Stack f [] $ reverse u ++ d

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
myManageHook = composeAll
    [ className =? "qutebrowser" --> unfloat
    , className =? "TeamViewer"  --> unfloat
    -- , className =? "Spotify"     --> doFloat
    -- , title     =? "todo"        --> doFloat
    ]
    <+> floatNextHook
    <+> (namedScratchpadManageHook scratchpads)
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
myEventHook =
    screenCornerEventHook <+> ewmhDesktopsEventHook <+> fullscreenEventHook

-- hide NSP ws
-- other ws make clickable with xdotool
formatWs "NSP"  = ""
formatWs wsName = xmobarAction ("xdotool key Super_L+" ++ wsIdx) "1" wsName
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
    { ppOutput  = hPutStrLn xmproc . xmobarShorten (ifHnsTop cfg 64 100)
    , ppCurrent = xmobarColor (cl_lilly cfg) "" .formatWs
    , ppHidden  = formatWs
    , ppTitle   = xmobarColor (cl_lilly cfg) ""
    , ppTitleSanitize = Prelude.filter (`elem` xmobarTitleAllowedChars) . xmobarStrip
    , ppUrgent  = xmobarColor (cl_aqua  cfg) "" . formatWs
    , ppOrder   = toOrdr
    , ppSep     = " | "
    , ppVisible = xmobarColor (cl_green cfg) ""
    , ppExtras = [willFloatAllNewPP id]
    } >>= dynamicLogWithPP
  where
    toOrdr (wsNames:layoutName:windowTitle:xtras:_) = [scrollableWsNames wsNames,xtras,windowTitle]
    toOrdr (wsNames:layoutName:windowTitle:_) = [scrollableWsNames wsNames,windowTitle]

scrollableWsNames wsNames = xmobarAction "xdotool key Super_L+Shift+Tab" "5" (xmobarAction "xdotool key Super_L+Tab" "4" wsNames)

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
mouseHelpActions = [
    ("Cancel menu", return ())
  , ("Kill"      , kill1)
  , ("Promote"    , promote)
  , ("Next layout", sendMessage NextLayout)
  , ("Inc Master", sendMessage $ IncMasterN (-1))
  , ("Dec Master", sendMessage $ IncMasterN 1)
  , ("Expand", sendMessage Expand)
  , ("Shrink", sendMessage Shrink)
  , ("magnify", namedScratchpadAction scratchpads "kmag")
  , ("copyToAll", windows copyToAll)
  , ("killAllOtherCopies", killAllOtherCopies)
  ]
myStartupHook cfg = do
    addScreenCorner SCUpperRight $ gsWithWindows mouseHelpActions cfg

main = do
  xmobarproc <- spawnPipe "~/.local/bin/xmobar-afreak"
  cfg <- getConfig
  xmonad . ewmh . ( withUrgencyHook NoUrgencyHook ) . MD.docks $ defaults xmobarproc cfg

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
        borderWidth        = 6,
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
        focusedBorderColor = cl_lilly cfg,
      -- key bindings
        keys               = myKeys cfg,
        mouseBindings      = myMouseBindings,
      -- hooks, layouts
        layoutHook         = refocusLastLayoutHook $ myLayout cfg,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook <+> (refocusLastWhen isFloat),
        logHook            = (myLogHook xmobarproc cfg), -- <+> refocusLastLogHook,
        startupHook        = myStartupHook cfg
    }

