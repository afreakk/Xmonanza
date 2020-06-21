import XMonad as XM
import System.Exit
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.StackSet as SS
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Renamed
import XMonad.Actions.WorkspaceNames
import XMonad.Prompt

import AConfig (getConfig, AConfig (..))

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Prompt theme
myXPConfig :: XPConfig
myXPConfig = def
    { font                = cl_font getConfig
    , bgColor             = cl_black getConfig
    , fgColor             = cl_grey getConfig
    , bgHLight            = cl_black getConfig
    , fgHLight            = cl_lilly getConfig
    , borderColor         = cl_lilly getConfig
    , promptBorderWidth   = 1
    , height              = fromIntegral $ cl_barHeight getConfig
    , position            = Bottom
    , historySize         = 100
    , historyFilter       = deleteConsecutive
    , autoComplete        = Nothing
    }

cmdSetVolume arg = "~/bin/setSinkVolumeDefault.sh " ++ arg
cmdMaimSelect out = "maim --select --hidecursor --format png " ++ out
cmdPipeImgToClip = " | xclip -selection clipboard -t image/png -i"
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XM.modMask = modm}) = M.fromList $
    [ ((modm.|.shiftMask,xK_Return), spawn $ XM.terminal conf)
    , ((shiftMask,      xK_Print ), spawn $ cmdMaimSelect "/dev/stdout" ++ cmdPipeImgToClip ++ "&& xclip -selection clipboard -t image/png -o | feh -")
    , ((0,              xK_Print ), spawn $ cmdMaimSelect "/dev/stdout" ++ cmdPipeImgToClip)
    , ((modm,           xK_Print ), spawn $ cmdMaimSelect "~/img.png")
      -- XF86AudioRaiseVolume
    , ((0,            0x1008ff13 ), spawn $ cmdSetVolume "+5%")
      -- XF86AudioLowerVolume
    , ((0,            0x1008ff11 ), spawn $ cmdSetVolume "-5%")
    , ((modm,               xK_o ), spawn "xmodmap ~/.Xmodmap")
    , ((modm,               xK_p ), spawn "clipmenu")
    , ((modm,               xK_s ), spawn "~/bin/openTerminalWithCurrentPwd.sh")
    , ((modm,               xK_f ), spawn "~/bin/dwmwindowselector.sh")
    , ((modm,               xK_y ), spawn "~/bin/terminal.sh")
    , ((modm,               xK_w ), spawn "~/bin/runner.sh")
    , ((modm,               xK_r ), renameWorkspace myXPConfig)
    , ((modm,               xK_q ), kill)
    , ((modm,               xK_d ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_d ), setLayout $ XM.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm .|. shiftMask, xK_r     ), refresh)
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
    -- Push window back into tiling
    , ((modm,               xK_u     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_grave     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm              , xK_grave     ), spawn "xmonad-afreak --recompile; xmonad-afreak --restart")
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_h ), spawn ("grep 'xK_' ~/coding/xmonanza/xmonad/Main.hs | dmenu -l 42"))
    ]
    ++
    [((m .|. modm, k), f i)
        | (i, k) <- zip workspaceNames workspaceKeys
        , (f, m) <- [(toggleOrView, 0), ((windows . W.shift), shiftMask)]]
    -- ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

workspaceNames = map show $ [1..9] ++ [0]
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

myTabConfig = def
  { activeTextColor = cl_black getConfig 
  , inactiveTextColor = cl_lilly getConfig 
  , activeColor = cl_lilly getConfig 
  , activeBorderColor = cl_lilly getConfig 
  , inactiveColor = cl_black getConfig 
  , inactiveBorderColor = cl_lilly getConfig 
  , urgentColor = cl_aqua getConfig 
  , urgentBorderColor = cl_lilly getConfig 
  , fontName = cl_font getConfig
  , decoHeight = fromIntegral $ cl_barHeight getConfig
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
myLayout = (renamed [Replace "B"](tabbedBottom shrinkText myTabConfig )) ||| renamed [Replace "T"] tiled ||| renamed [Replace "MT"] (Mirror tiled)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

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
myManageHook = composeAll []
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

------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook xmproc = do
  workspaceNamesPP xmobarPP
    { ppOutput  = hPutStrLn xmproc
    , ppCurrent = xmobarColor (cl_lilly getConfig) ""
    , ppHidden  = clickableWs
    , ppTitle   = xmobarColor (cl_lilly getConfig) "" . shorten 45
    , ppUrgent  = xmobarColor (cl_aqua  getConfig) "" . clickableWs
    , ppSep = " | "
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
  xmobarproc <- spawnPipe "/home/afreak/.local/bin/xmobar-afreak"
  xmonad (withUrgencyHook NoUrgencyHook (ewmh (docks (defaults xmobarproc))))

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
defaults xmobarproc = def {
      -- simple stuff
        terminal           = "alacritty",
        focusFollowsMouse  = True,
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
        normalBorderColor  = cl_black getConfig,
        focusedBorderColor = cl_grey getConfig,
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
      -- hooks, layouts
        layoutHook         = smartBorders.avoidStruts $ myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook xmobarproc,
        startupHook        = myStartupHook
    }

