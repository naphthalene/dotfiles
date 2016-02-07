{-# LANGUAGE UnicodeSyntax #-}
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import System.Exit
import XMonad.Config.Xfce
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.SpawnOn
-- import XMonad.Hooks.FadeWindows
import XMonad.Layout.PerWorkspace
import qualified XMonad.Actions.FlexibleResize as Flex
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

--myTerminal      = "aterm -tr +sb -sh 40 -fg white -fn -misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-1"
-- myTerminal      = "urxvt +sb -rv -ss +j +si -fn -misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-1"
dynamicTerm     = "urxvt +sb -rv -ss"

myBorderWidth   = 1

myModMask       = mod4Mask

myNumlockMask   = mod2Mask

myWorkspaces    = ["trm","web","prg","cht","etc","gme","min","viz","mus"]

myNormalBorderColor  = "#99ccaa"
myFocusedBorderColor = "#000000"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn dynamicTerm)

    -- launch a terminal
    , ((modm .|. controlMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

      -- launch emacs
    , ((modm,               xK_o     ), spawn "emacs")

      -- launch firefox
    , ((modm,               xK_f     ), spawn "firefox")

      -- launch xkill
    , ((modm,               xK_x     ), spawn "xkill")

      -- launch xrandr --auto
    , ((modm .|. controlMask, xK_x     ), spawn "xrandr --auto")

    -- Take a screenshot with scrot
    , ((0,              xK_Print     ), spawn "scrot -e 'mv $f ~/pictures/shots/'")

    , ((modm .|. controlMask, xK_7  )
      , spawn "/home/sigmat/bin/spotifystat notify")
    , ((modm .|. controlMask, xK_8  )
      , spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ((modm .|. controlMask, xK_9  )
      , spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous; spotifystat notify")
    , ((modm .|. controlMask, xK_0  )
      , spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next; spotifystat notify")


    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "dmenu_run")

    -- Lock screen
    , ((modm .|. shiftMask, xK_l     ), spawn "xflock4")

          -- raise volume
    , ((0, xF86XK_AudioRaiseVolume     ), spawn "amixer sset Master 2%+")

    -- lower volume
    , ((0, xF86XK_AudioLowerVolume     ), spawn "amixer sset Master 2%-")

    --
    , ((0, xF86XK_AudioPlay            ), spawn "mpc toggle")
    , ((0, xF86XK_AudioPrev            ), spawn "mpc prev")
    , ((0, xF86XK_AudioNext            ), spawn "mpc next")
    , ((0, xF86XK_AudioStop            ), spawn "mpc toggle")

    -- volume+
    , ((modm,           xK_equal     ), spawn "amixer sset Master 2%+")
    -- volume-
    , ((modm,           xK_minus     ), spawn "amixer sset Master 2%-")

    , ((modm,           xK_u         ), spawn "touchpad.sh")

      -- reset volume to zero
    , ((modm,                    xK_0), spawn "aumix -v-100")

    -- reset volume to max
    , ((modm,            xK_backslash), spawn "aumix -L")

    -- close focused window
    , ((modm .|. shiftMask,      xK_c), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap (used with avoidStruts from Hooks.ManageDocks)
    , ((modm , xK_b ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), spawn "xfce4-session-logout")

    -- Restart xmonad
    , ((modm              , xK_q     ), restart "xmonad" True)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

      -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

      -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

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
myLayout = onWorkspace "prg" emacsLayout $ onWorkspace "web" webLayout $ standardLayout
  where
    standardLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
        where
          tiled   = Tall nmaster delta ratio
          nmaster = 1
          ratio   = 14/25
          delta   = 3/100
    webLayout = avoidStruts $ (Mirror tiled ||| tiled ||| Full)
        where
          tiled = Tall nmaster delta ratio
          nmaster = 1
          ratio   = 22/25
          delta   = 2/100
    emacsLayout = avoidStruts $ (Mirror tiled ||| tiled ||| Full)
        where
          tiled = Tall nmaster delta ratio
          nmaster = 1
          ratio   = 3/4
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
myManageHook = composeAll
    [ className =? "Xfrun4"                  --> doFloat
    , className =? "Orage"                   --> doFloat
    , className =? "processing-app-Base"     --> doFloat
    , className =? "emacs"                   --> doShift "prg"
    , className =? "firefox"                 --> doShift "web"
    , manageDocks
    ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
-- myLogHook = return () --dynamicLogWithPP dzenPP

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = spawn "/home/sigmat/.xmonad/startup.sh"


-- myFadeHook = composeAll [isUnfocused --> transparency 0.2
--                         ,                opaque
--                         ]
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput = dbusOutput dbus
    , ppTitle = pangoSanitize
    , ppCurrent = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden = const ""
    , ppUrgent = pangoColor "red"
    , ppLayout = const ""
    , ppSep = " "
    }


getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = D.emit dbus signal
    where signal = (D.signal
                         (D.objectPath_    "/org/xmonad/Log")
                         (D.interfaceName_ "org.xmonad.Log")
                         (D.memberName_    "Update")) {
                     D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
                   }

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>' xs = "&gt;" ++ xs
    sanitize '<' xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&' xs = "&amp;" ++ xs
    sanitize x xs = x:xs


-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  dbus ← D.connectSession
  getWellKnownName dbus
  xmonad xfceConfig {
      -- simple stuff
      terminal           = dynamicTerm,
      focusFollowsMouse  = myFocusFollowsMouse,
      borderWidth        = myBorderWidth,
      modMask            = myModMask,
      workspaces         = myWorkspaces,
      normalBorderColor  = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,

      -- key bindings
      keys               = myKeys,
      mouseBindings      = myMouseBindings,

      -- hooks, layouts
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      logHook            = ewmhDesktopsLogHook
                           <+> dynamicLogWithPP (prettyPrinter dbus)
                           -- <+> fadeWindowsLogHook myFadeHook
             ,
      handleEventHook    = ewmhDesktopsEventHook
                           <+> fullscreenEventHook
                           <+> docksEventHook
                           -- <+> fadeWindowsEventHook
             ,
      startupHook        = ewmhDesktopsStartup
                           <+> myStartupHook
    }
