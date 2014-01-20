--
-- My xmonad configuration file.
--
--
import XMonad hiding ((|||))

import XMonad.Actions.DynamicWorkspaces -- For creating/deleting workspaces dynamically.

import XMonad.Hooks.ManageDocks -- For managing specific windows.
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.FadeInactive -- Make inactive windows transparent.
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import XMonad.Layout.Decoration -- For title bars for windows.
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.Minimize
import XMonad.Layout.Monitor
import XMonad.Layout.MultiToggle
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
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.Util.WorkspaceCompare

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Monoid (All(..))
import Data.Time

import System.Exit
import System.IO
import System.Locale

-- Colour configurations
myInactiveBorderColor = "#656555"
myActiveBorderColor = myInactiveTextColor
myActiveColor = "#94bff3"
myInactiveColor = "#5f5f5f"
myActiveTextColor = "Black"
myInactiveTextColor = "#dcdccc"
myBackgroundColor = "#3f3f3f"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here. Everything is emacs-style.
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
  ++
  -- mod-[1..9] switches to the appropriate workspace.
  map (\x -> ("M-" ++ show (x+1), withNthWorkspace W.greedyView x)) [0..8]

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

myDecoTheme = defaultThemeWithImageButtons { 
  fontName = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
  }

myHandleEventHook = minimizeEventHook
myLayoutHook = myLayoutModifiers (tiled ||| Mirror tiled ||| Full)
  where
    -- default layout modifiers to be applied everywhere
    myLayoutModifiers = (renamed [CutWordsLeft 3] . layoutHints . smartBorders . minimize . avoidStruts . (imageButtonDeco shrinkText myDecoTheme)) 
    -- Default tiling algorithm partitions the screen into two panes.
    tiled   = Tall 1 (3/100) (1/2)

------------------------------------------------------------------------
-- Window rules:

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
    override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
    wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
    return $ maybe False (elem $ fromIntegral override) wt

checkAlwaysTop :: Query Bool
checkAlwaysTop = ask >>= \w -> liftX $ do
  top <- getAtom "_NET_WM_STATE_STAYS_ON_TOP"
  mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
  case mbr of
    Just rs -> return $ any (`elem` [top]) (map fromIntegral rs)
    _       -> return False

topEventHook :: Event -> X All
topEventHook (MapNotifyEvent {ev_window = w}) = do
  whenX ((not `fmap` (isClient w)) <&&> runQuery checkAlwaysTop w) refresh
  return (All True)
topEventHook _ = return (All True)

manageAlwaysTop :: ManageHook
manageAlwaysTop = checkAlwaysTop --> doFloat

myManageHook = composeAll . concat $
               [ [className =? c --> doFloat <+> doF W.swapDown | c <- myFloats],
                 [className =? "Wine" --> doFloat <+> doShift "netflix"],
                 [kdeOverride --> doFloat <+> doF W.swapDown]
               ]
  where
    myFloats = ["SMPlayer", "MPlayer", "Krunner", "Plugin-container", "Redshift GUI"]

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

myPPExtras :: [X (Maybe String)]
myPPExtras = []

myDzenPPConfig :: Handle -> PP
myDzenPPConfig h = defaultPP
                   { ppOutput   = hPutStrLn h
                   , ppCurrent  = dzenColor myActiveTextColor myActiveColor . pad
                   , ppExtras   = map (dzenColorL "" myBackgroundColor . padL) myPPExtras
                   , ppHidden   = dzenColor myInactiveTextColor myInactiveColor . pad . dzenSwitchWs
                   , ppLayout   = dzenColor "#dca3a3" myBackgroundColor . pad . dzenSwitchLayout
                   , ppOrder    = \(ws:l:t:xs) -> (l:ws:xs) ++ [t]
                   , ppSep      = " "
                   , ppTitle    = dzenColor "#bfebbf" myBackgroundColor . pad . dzenEscape . shorten 80
                   , ppUrgent   = dzenColor myActiveTextColor myBackgroundColor . pad . dzenEscape . shorten 80 . dzenStrip
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

main = do
  myDzenInstance <- spawnPipe myDzenBar
  xmonad $ ewmh defaultConfig {
    -- simple stuff
    terminal           = "konsole", --default terminal
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = 2,
    modMask            = mod4Mask, --default mod key (mod4 is usually the win key)
    workspaces         = ["home", "math", "web"], --default workspaces that xmonad starts with
    normalBorderColor  = myInactiveBorderColor,
    focusedBorderColor = myActiveBorderColor,
    
    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    
    -- hooks, layouts
    layoutHook         = myLayoutHook,
    handleEventHook    = myHandleEventHook,
    manageHook         = manageDocks <+> myManageHook,
    logHook            = myLogHook >> (dynamicLogWithPP $ myDzenPPConfig myDzenInstance)
    }
    
