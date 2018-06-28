-- Needed for Layout
{-# LANGUAGE FlexibleContexts #-}

-- XMonad Core
import qualified XMonad.Config               as Config
import qualified XMonad.Core                 as Core
import qualified XMonad.Layout               as Layout
import qualified XMonad.Main                 as Main
import qualified XMonad.ManageHook           as ManageHook
import qualified XMonad.Operations           as Operations

-- XMonad Contrib
import qualified XMonad.Config.Desktop       as Desktop
import qualified XMonad.Hooks.SetWMName      as SetWMName
import qualified XMonad.Layout.NoBorders     as NoBorders
import qualified XMonad.StackSet             as StackSet
import qualified XMonad.Util.NamedScratchpad as NamedScratchpad

-- X11 keyboard symbols
import           Graphics.X11

-- Haskell
import qualified Data.Char                   as Char
import qualified Data.List                   as List
import qualified Data.Map                    as Map
import qualified Data.Monoid                 as Monoid
import qualified System.Exit                 as Exit

-- Operators
import           Data.Bits                   ((.|.))
import           XMonad.Layout               ((|||))
import           XMonad.ManageHook           ((-->), (=?))

main =
  Main.xmonad $
  Desktop.desktopConfig
    { Core.modMask = mod3Mask
    , Core.terminal = "urxvtc"
    , Core.keys = keys
    , Core.layoutHook = layoutHook
    , Core.focusFollowsMouse = False
    , Core.normalBorderColor = "#073642"
    , Core.focusedBorderColor = "#586e75"
    , Core.manageHook = manageHook
    , Core.workspaces = workspaces
    , Core.startupHook =
        Core.startupHook Desktop.desktopConfig >> SetWMName.setWMName "LG3D"
    }

manageHook =
  Monoid.mconcat
    [ Core.manageHook Desktop.desktopConfig
    , NamedScratchpad.namedScratchpadManageHook myScratchpads
    -- KDE floating windows
    , className "plasma-desktop" --> ManageHook.doFloat
    , className "plasmashell" --> ManageHook.doFloat
    ]
  where
    className c = (fmap . fmap) Char.toLower ManageHook.className =? c

workspaces = ["7", "8", "9", "0"]

layoutHook =
  Desktop.desktopLayoutModifiers $
  NoBorders.smartBorders (tall ||| Layout.Mirror tall)
  where
    tall =
      Layout.Tall
        { Layout.tallNMaster = 1
        , Layout.tallRatioIncrement = 3 / 100
        , Layout.tallRatio = 1 / 2
        }

toggleScratchpad = NamedScratchpad.namedScratchpadAction myScratchpads

keys conf =
  Map.fromList $
  [ ((mod3Mask .|. shiftMask, xK_Return), Core.spawn $ Core.terminal conf) -- %! Launch terminal
  , ((mod3Mask .|. mod1Mask, xK_w), toggleScratchpad "keepassxc")
  , ((mod3Mask .|. mod1Mask, xK_l), toggleScratchpad "google-chrome")
  -- Launch dmenu
  , ((mod3Mask, xK_minus), Core.spawn "krunner")
  -- Close the focused window
  , ((mod3Mask .|. shiftMask, xK_c), Operations.kill)
  -- Rotate through the available layout algorithms
  , ((mod3Mask, xK_space), Operations.sendMessage Layout.NextLayout)
  -- Reset the layouts on the current workspace to default
  , ( (mod3Mask .|. shiftMask, xK_space)
    , Operations.setLayout $ Core.layoutHook conf)
  -- Resize viewed Operations.windows to the correct size
  , ((mod3Mask, xK_n), toggleScratchpad "mid")
  -- Take screenshots
  , ((mod3Mask, xK_p), toggleScratchpad "spectacle")
  -- Move focus to the next window
  , ((mod3Mask, xK_Tab), Operations.windows StackSet.focusDown)
  -- Move focus to the previous window
  , ((mod3Mask .|. shiftMask, xK_Tab), Operations.windows StackSet.focusUp)
  -- Move focus to the next window
  , ((mod3Mask, xK_j), Operations.windows StackSet.focusDown)
  -- Move focus to the previous window
  , ((mod3Mask, xK_k), Operations.windows StackSet.focusUp)
  -- Move focus to the master window
  , ((mod3Mask, xK_m), Operations.windows StackSet.focusMaster)
  -- Move focus to the master window
  , ((mod3Mask, xK_s), toggleScratchpad "top")
  , ((mod3Mask .|. mod1Mask, xK_s), toggleScratchpad "bottom")
  -- Swap the focused window and the master window
  , ((mod3Mask, xK_Return), Operations.windows StackSet.swapMaster)
  -- Swap the focused window with the next window
  , ((mod3Mask .|. shiftMask, xK_j), Operations.windows StackSet.swapDown)
  -- Swap the focused window with the previous window
  , ((mod3Mask .|. shiftMask, xK_k), Operations.windows StackSet.swapUp)
  -- Shrink the master area
  , ((mod3Mask, xK_h), Operations.sendMessage Layout.Shrink)
  -- Expand the master area
  , ((mod3Mask, xK_l), Operations.sendMessage Layout.Expand)
  -- Push window back into tiling
  , ( (mod3Mask, xK_t)
    , Operations.withFocused $ Operations.windows . StackSet.sink)
  -- Increment the number of Operations.windows in the master area
  , ((mod3Mask, xK_comma), Operations.sendMessage (Layout.IncMasterN 1))
  -- Deincrement the number of windows in the master area
  , ((mod3Mask, xK_period), Operations.sendMessage (Layout.IncMasterN (-1)))
  -- Quit XMonad
  , ((mod3Mask .|. shiftMask, xK_q), Core.io (Exit.exitWith Exit.ExitSuccess))
  -- Restart XMonad
  , ((mod3Mask, xK_q), Core.spawn "xmonad --recompile && xmonad --restart")
  ] ++
  -- project workspaces
  [ ( (mod3Mask .|. mod1Mask, key)
    , Operations.windows $ StackSet.greedyView workspace)
  | (workspace, key) <-
      [ ("work-mail", xK_m)
      , ("work-calendar", xK_c)
      , ("work-drive", xK_d)
      , ("work-admin", xK_a)
      , ("google-chrome", xK_g)
      ]
  ] ++
  -- mod-[1..9]: Switch to workspace N
  [ ((mod3Mask, key), Operations.windows $ StackSet.greedyView workspace)
  | (workspace, key) <- zip (Core.workspaces conf) [xK_7, xK_8, xK_9, xK_0]
  ] ++
  -- mod-shift-[1..9]: Move client to workspace N
  [ ( (mod3Mask .|. shiftMask, key)
    , Operations.windows $ StackSet.shift workspace)
  | (workspace, key) <- zip (Core.workspaces conf) [xK_7, xK_8, xK_9, xK_0]
  ] ++
  -- mod-{w,e,r}: Switch to physical/Xinerama screens 1, 2
  [ ( (mod3Mask, key)
    , Operations.screenWorkspace sc >>=
      flip Core.whenJust (Operations.windows . StackSet.view))
  | (key, sc) <- zip [xK_1, xK_2] [0 ..]
  ] ++
  -- mod-shift-{w,e,r}: Move client to screen 1, 2, or 3
  [ ( (mod3Mask .|. shiftMask, key)
    , Operations.screenWorkspace sc >>=
      flip Core.whenJust (Operations.windows . StackSet.shift))
  | (key, sc) <- zip [xK_1, xK_2] [0 ..]
  ]

myScratchpads =
  [ NamedScratchpad.NS
      "google-chrome"
      "google-chrome"
      (ManageHook.appName =? "google-chrome")
      (centeredLayout defaultMargin)
  , NamedScratchpad.NS
      "mid"
      "urxvtmux mid"
      (ManageHook.appName =? "mid")
      (centeredLayout 0)
  , NamedScratchpad.NS
      "top"
      "urxvtmux top"
      (ManageHook.appName =? "top")
      (topLayout 0.03)
  , NamedScratchpad.NS
      "bottom"
      "urxvtmux bottom"
      (ManageHook.appName =? "bottom")
      (bottomLayout 0.03)
  , NamedScratchpad.NS
      "keepassxc"
      "keepassxc"
      (ManageHook.appName =? "keepassxc")
      (centeredLayout 0.20)
  , NamedScratchpad.NS
      "spectacle"
      "spectacle"
      (ManageHook.appName =? "spectacle")
      (centeredLayout 0.30)
  ]
  where
    defaultMargin = 0.035
    widescreen margin = margin * 10 / 16
    opposite margin = 1 - 2 * margin
    centeredLayout margin =
      NamedScratchpad.customFloating $
      StackSet.RationalRect
        (widescreen margin)
        margin
        (opposite $ widescreen margin)
        (opposite margin)
    topLayout margin =
      NamedScratchpad.customFloating $
      StackSet.RationalRect
        (widescreen margin)
        margin
        (opposite $ widescreen margin)
        (0.5 - 1.5 * margin)
    bottomLayout margin =
      NamedScratchpad.customFloating $
      StackSet.RationalRect
        (widescreen margin)
        (0.5 + 0.5 * margin)
        (opposite $ widescreen margin)
        (0.5 - 1.5 * margin)
