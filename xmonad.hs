--
-- My xmonad configuration file.
--
--

import XMonad hiding ((|||))

import XMonad.Actions.DynamicWorkspaces -- For creating/deleting workspaces dynamically.

import XMonad.Config.Kde -- KDE-specific tweaks.

import XMonad.Hooks.ManageDocks -- For managing specific windows.
import XMonad.Hooks.Minimize
import XMonad.Hooks.FadeInactive -- Make inactive windows transparent.
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import XMonad.Layout.Decoration -- For title bars for windows.
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Minimize
import XMonad.Layout.Monitor
import XMonad.Layout.NoFrillsDecoration -- For title bars for windows.
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed

import XMonad.Prompt
import XMonad.Prompt.FuzzyShell

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig -- More intuitive keybinding configuration
import XMonad.Util.Loggers
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Themes
import XMonad.Util.WorkspaceCompare

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Time

import System.Exit
import System.IO
import System.Locale

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "konsole"

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
--myNumlockMask   = mod2Mask

-- Workspaces

myWorkspaces = ["home", "web", "math"]


-- Border configurations
myBorderWidth   = 2

-- Colour configurations
myInactiveBorderColor = "#656555"
myActiveBorderColor = myInactiveTextColor
myActiveColor = "#94bff3"
myInactiveColor = "#5f5f5f"
myActiveTextColor = "Black"
myInactiveTextColor = "#dcdccc"
myBackgroundColor = "#3f3f3f"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys = \conf -> mkKeymap conf 
  $ [ ("M-S-<Return>", spawn $ XMonad.terminal conf), -- launch a terminal
      ("M-r", fuzzyShellPrompt greenXPConfig), -- launch shell prompt
      ("M-S-c", kill), -- close focused window 
      ("M-<Space>", sendMessage NextLayout), -- Rotate through the available layout algorithms
      ("M-f", sendMessage $ JumpToLayout "Full"), -- Full layout
      ("M-S-<Space>", setLayout $ XMonad.layoutHook conf), -- Reset to default layouts on the current workspace
      ("M-n", refresh), -- Resize viewed windows to the correct size
      ("M-<Tab>", windows W.focusDown), -- Move focus to the next window
      ("M-S-<Tab>", windows W.focusUp), -- Move focus to the previous window
      ("M-m", windows W.focusMaster), -- Move focus to the master window
      ("M-<Return>", windows W.swapMaster), -- Swap the focused window and the master window
      ("M-S-j", windows W.swapDown), -- Swap the focused window with the next window
      ("M-S-k", windows W.swapUp), -- Swap the focused window with the previous window
      ("M-h", sendMessage Shrink), -- Shrink the master area
      ("M-l", sendMessage Expand), -- Expand the master area
      ("M-t", withFocused $ windows . W.sink), -- Push window back into tiling
      ("M-,", sendMessage (IncMasterN 1)), -- Increment the number of windows in the master area
      ("M-.", sendMessage (IncMasterN (-1))), -- Deincrement the number of windows in the master area
      ("M-b", sendMessage ToggleStruts), -- Toggle the status bar gap
      ("M-S-q", io (exitWith ExitSuccess)), -- Quit xmonad
      ("M-q", restart "xmonad" True), -- Restart xmonad
      ("M-S-<Backspace>", removeWorkspace), -- Remove workspace
      ("M-w", selectWorkspace defaultXPConfig), -- Select workspace to navigate to
      ("M-S-w", withWorkspace defaultXPConfig (windows . W.shift)), -- Move current window to selected workspace
      ("M-S-r", renameWorkspace defaultXPConfig) -- Rename workspace
     ]

  -- ++
  --   -- mod-[1..9], Switch to workspace N
  --   --mod-shift-[1..9], Move client to workspace N
  -- [((m .|. modMask, k), windows $ f i)
  --        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  --   , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [((modMask, button1), (\w -> focus w >> mouseMoveWindow w)), -- mod-button1, Set the window to floating mode and move by dragging
   ((modMask, button2), (\w -> focus w >> windows W.swapMaster)), -- mod-button2, Raise the window to the top of the stack
   ((modMask, button3), (\w -> focus w >> mouseResizeWindow w)) -- mod-button3, Set the window to floating mode and resize by dragging
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

myDecoTheme = defaultThemeWithImageButtons { 
  fontName = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
  }


myHandleEventHook = minimizeEventHook
myLayoutHook = myLayoutModifiers (tiled ||| Mirror tiled ||| Full)
  where
    -- default layout modifiers to be applied everywhere
    myLayoutModifiers = (renamed [CutWordsLeft 2] . smartBorders . minimize . avoidStruts . (imageButtonDeco shrinkText myDecoTheme)) 
    -- Default tiling algorithm partitions the screen into two panes.
    tiled   = Tall 1 (3/100) (1/2)

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
myManageHook = composeAll . concat $
               [ [className =? c --> doFloat | c <- myFloats],
                 [className =? c --> doIgnore | c <- myIgnores],
                 [resource  =? c --> doIgnore | c <- myIgnores]]
                   where
                     myFloats = ["SMPlayer", "MPlayer", "Krunner", "Vlc"]
                     myIgnores = ["desktop_window", "kdesktop", "trayer"]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True


------------------------------------------------------------------------
-- Dzen configuration
-- Notice the addition to logHook in the main function.
dzenSwitchWs :: String -> String
dzenSwitchWs s = "^ca(1,switch-to-workspace.zsh " ++ (show s) ++ ")" ++ s ++ "^ca()"

dzenSwitchLayout :: String -> String
dzenSwitchLayout = wrap "^ca(1,xdotool key Super_L+space)" "^ca()"

-- World clock for display in Dzen. This code is somewhat temporary.
myWorldLocations = [("New York", -300), ("Pune", 330)]

myTimeFormatString :: String
myTimeFormatString = "%Z: %R" -- Name of zone: Time

utcToFormattedTime :: UTCTime -> TimeZone -> String
utcToFormattedTime u = (formatTime defaultTimeLocale myTimeFormatString) . (flip utcToZonedTime u)

myWorldClock :: Logger
myWorldClock = let tzones = map (\(a,b) -> TimeZone b False a) myWorldLocations
               in io $ 
                  do currentUTC <- getZonedTime >>= return . zonedTimeToUTC
                     return $ Just $ intercalate " ∙ " $ map (utcToFormattedTime currentUTC) tzones

myPPExtras :: [X (Maybe String)]
myPPExtras = [myWorldClock]

myDzenPPConfig :: Handle -> PP
myDzenPPConfig h = defaultPP
                   { ppOutput   = hPutStrLn h
                   , ppCurrent  = dzenColor myActiveTextColor myActiveColor . pad
                   , ppExtras   = map (dzenColorL "" myBackgroundColor . padL) myPPExtras
                   , ppHidden   = dzenColor myInactiveTextColor myInactiveColor . pad . dzenSwitchWs
                   , ppLayout   = dzenColor "#dca3a3" myBackgroundColor . pad . dzenSwitchLayout
                   , ppOrder    = \(ws:l:t:xs) -> (l:ws:xs) ++ [t]
                   , ppSep      = " "
                   , ppSort     = getSortByTag
                   , ppTitle    = dzenColor "#bfebbf" myBackgroundColor . pad . dzenEscape . shorten 80
                   , ppWsSep    = "|"
                   }

myDzenBar :: String
myDzenBar = "dzen2 -ta l -fg grey80 -bg grey20"

------------------------------------------------------------------------
-- Final loghook, including dzen and fading (transparency of windows).
myLogHook :: X()
myLogHook = fadeInactiveLogHook fadeAmount >>
            setWMName "LG3D"
                where fadeAmount = 0.90

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  myDzenInstance <- spawnPipe myDzenBar
  xmonad $ ewmh defaultConfig {
      -- simple stuff
      terminal           = myTerminal,
      focusFollowsMouse  = myFocusFollowsMouse,
      borderWidth        = myBorderWidth,
      modMask            = myModMask,
      --numlockMask        = myNumlockMask,
      workspaces         = myWorkspaces,
      normalBorderColor  = myInactiveBorderColor,
      focusedBorderColor = myActiveBorderColor,
      
      -- key bindings
      keys               = myKeys,
      mouseBindings      = myMouseBindings,

      -- hooks, layouts
      layoutHook         = myLayoutHook,
      handleEventHook    = myHandleEventHook,
      manageHook         = manageHook kde4Config <+> myManageHook,
      logHook            = myLogHook >> (dynamicLogWithPP $ myDzenPPConfig myDzenInstance)
      }
         
