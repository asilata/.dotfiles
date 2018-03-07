{-#LANGUAGE FlexibleContexts, TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses#-}
--
-- My xmonad configuration file.
--
--
import XMonad hiding ((|||))

import XMonad.Actions.DynamicWorkspaces -- For creating/deleting workspaces dynamically.
import XMonad.Actions.MouseGestures

import XMonad.Hooks.ManageDocks -- For managing specific windows.
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.FadeInactive -- Make inactive windows transparent.
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import XMonad.Layout.BoringWindows
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutHints
import XMonad.Layout.Minimize
import XMonad.Layout.Monitor
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.Shell

import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig -- More intuitive keybinding configuration
--import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Themes
import XMonad.Util.WorkspaceCompare

import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as M
import Data.Monoid (All(..))
import Data.Time

import System.Exit
import System.IO
import System.Locale

-- Color configurations
myFgColor = "#DCDCCC"
myBgColor = "#3F3F3F"
myHighlightedFgColor = myFgColor
myHighlightedBgColor = "#2B2B2B"

--- Borders
myActiveBorderColor = "#94BFF3"
myInactiveBorderColor = "#7CB8BB"
myBorderWidth = 10

-- Font
myFont :: String
myFont = "xft:UbuntuMono:size=14"

-- new XPConfig (for shell prompt, tab bars, etc)
myXPConfig :: XPConfig
myXPConfig = def {
  font = myFont,
  fgColor = myFgColor,
  bgColor = myBgColor,
  fgHLight = myHighlightedFgColor,
  bgHLight = myHighlightedBgColor,
  borderColor = myActiveBorderColor,
  height = 40}

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here. Everything is emacs-style.
--
myKeys = \conf -> mkKeymap conf 
  $ [ ("M-S-<Return>", spawn $ XMonad.terminal conf), -- launch a terminal
      ("M-r", shellPrompt myXPConfig), -- launch shell prompt
      ("M-S-c", kill), -- close focused window 
      ("M-<Space>", sendMessage NextLayout), -- Rotate through the available layout algorithms
      ("M-f", sendMessage $ Toggle NBFULL), -- Toggle Full layout
      ("M-g", sendMessage $ Toggle GRIDIFY), -- Toggle Grid layout
      ("M-k", namedScratchpadAction myScratchPads "konsole"),
      ("M-S-<Space>", setLayout $ XMonad.layoutHook conf), -- Reset to default layouts on the current workspace
      ("M-n", refresh), -- Resize viewed windows to the correct size
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
      ("M-w", selectWorkspace myXPConfig), -- Select workspace to navigate to 
      ("M-S-w", withWorkspace myXPConfig (windows . W.shift)), -- Move current window to selected workspace
      ("M-S-r", renameWorkspace def) -- Rename workspace
     ]
  ++
  [
    ("M-C-h", sendMessage $ pullGroup L)
  , ("M-C-l", sendMessage $ pullGroup R)
  , ("M-C-k", sendMessage $ pullGroup U)
  , ("M-C-j", sendMessage $ pullGroup D)
    
  , ("M-C-m", withFocused (sendMessage . MergeAll))
  , ("M-C-u", withFocused (sendMessage . UnMerge))
  
  , ("M-C-.", onGroup W.focusUp')
  , ("M-C-,", onGroup W.focusDown')
  ]
  ++
  [
    ("M-<Tab>", focusDown)
  , ("M-S-<Tab>", focusUp)
  , ("M-m", focusMaster)
  ]
  ++
  -- mod-[1..9] switches to the appropriate workspace.
  map (\x -> ("M-" ++ show (x+1), withNthWorkspace W.greedyView x)) [0..8]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [((modMask, button1), (\w -> focus w >> mouseMoveWindow w)) -- mod-button1, Set the window to floating mode and move by dragging
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster)) -- mod-button2, Raise the window to the top of the stack
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w)) -- mod-button3, Set the window to floating mode and resize by dragging
   -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]
  ++
  [
    ((mod1Mask, button1), mouseGesture gestures)
  ]

gestures = M.fromList
  [ ([], sendMessage . UnMerge)
  , ([U], \_ -> sendMessage $ pullGroup U)
  , ([D], \_ -> sendMessage $ pullGroup D)
  , ([L], \_ -> sendMessage $ pullGroup L)
  , ([R], \_ -> sendMessage $ pullGroup R)
  ]

myDecoTheme = def { 
  fontName = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
  }

myHandleEventHook = minimizeEventHook <+> fullscreenEventHook
data GRIDIFY = GRIDIFY deriving (Read, Show, Eq, Typeable)
instance Transformer GRIDIFY Window where
  transform GRIDIFY x k = k (GridRatio (4/3)) (const x)

myLayoutHook = myLayoutModifiers (tall ||| threeColMid)
  where myLayoutModifiers = (
          layoutHints
          . smartBorders
          . minimize
          . avoidStruts
          . windowNavigation
          . boringAuto
          . (mkToggle (single NBFULL))
          . (mkToggle (single GRIDIFY))
          . subTabbed
          )
        tall = Tall 1 (3/100) (1/2)
        threeColMid = ThreeColMid 1 (1/100) (1/2)


------------------------------------------------------------------------
-- Window rules:

myScratchPads :: [NamedScratchpad]
myScratchPads = [
  NS "konsole" "konsole --profile Floating" (fmap ("Floating" `isPrefixOf`) title) (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]

myManageHook = composeAll . concat $
               [ [className =? c --> doFloat | c <- myFloats],
                 [className =? c --> doFullFloat | c <- myFullFloats]
               ]
  where
    myFloats = ["SMPlayer", "MPlayer", "Krunner", "Plugin-container", "Redshiftgui", "plasma", "Plasma", "plasma-desktop", "Plasma-desktop", "plasmashell", "Yakuake"]
    myFullFloats = ["Xournal"]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False


------------------------------------------------------------------------
-- Final loghook, including fading (transparency of windows).
myLogHook :: X()
myLogHook = fadeInactiveLogHook fadeAmount >>
            setWMName "LG3D"
                where fadeAmount = 0.90

------------------------------------------------------------------------

main = do
  xmonad $ docks $ ewmh def {
    -- simple stuff
    terminal           = "konsole", --default terminal
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = 5,
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
    manageHook         = myManageHook <+> namedScratchpadManageHook myScratchPads,
    logHook            = myLogHook
    }
    
