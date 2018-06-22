-- needed for Tabs
{-# LANGUAGE NoMonomorphismRestriction #-}
-- needed for Layout
{-# LANGUAGE FlexibleContexts #-}

import qualified XMonad.Config as Config
import qualified XMonad.Core as Core
import qualified XMonad.Layout as Layout
import qualified XMonad.Main as Main
import qualified XMonad.ManageHook as ManageHook
import qualified XMonad.Operations as Operations

import qualified System.Exit as Exit
import qualified XMonad.Actions.DynamicProjects as DynamicProjects
import qualified XMonad.Layout.NoBorders as NoBorders
import qualified XMonad.Layout.Tabbed as Tabbed
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Util.NamedScratchpad as NamedScratchpad

import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Environment as Environment
import qualified System.IO.Unsafe as Unsafe
import qualified Data.Monoid as Monoid

import Graphics.X11 hiding (refreshKeyboardMapping)

import Data.Bits ((.|.))
import XMonad.Layout ((|||))
import XMonad.ManageHook ((-->), (=?))

env = Unsafe.unsafePerformIO . Environment.getEnv

main =
  Main.xmonad $
  DynamicProjects.dynamicProjects projects $
  Config.def
    { Core.modMask = mod4Mask
    , Core.terminal = "urxvtc"
    , Core.focusedBorderColor = env "THEME_LIGHT_GREEN"
    , Core.normalBorderColor = env "THEME_BLACK"
    , Core.keys = keys
    , Core.layoutHook = layout
    , Core.focusFollowsMouse = True
    , Core.manageHook = managehook
    , Core.workspaces = workspaces
    }

managehook =
  Monoid.mconcat
    [ NamedScratchpad.namedScratchpadManageHook myScratchpads
    , fmap (List.isPrefixOf "google-chrome") ManageHook.appName -->
      ManageHook.doShift "google-chrome"
    ]

projects =
  [ wwwProject "work-mail" "work" "mail.google.com"
  , wwwProject "work-calendar" "work" "calendar.google.com"
  , wwwProject "work-drive" "work" "drive.google.com"
  , wwwProject "work-admin" "work" "admin.google.com"
  , wwwProject "work-groups" "work" "groups.google.com"
  ]
  where
    wwwProject name profile url =
      DynamicProjects.Project
        { DynamicProjects.projectName = name
        , DynamicProjects.projectDirectory = "~/"
        , DynamicProjects.projectStartHook =
            Just $ Core.spawn $ "www " ++ profile ++ " " ++ url
        }

workspaces =
  map show [1 .. 9] ++
  map DynamicProjects.projectName projects ++ ["google-chrome"]

tabTheme =
  Tabbed.defaultTheme
    { Tabbed.fontName =
        "xft:" ++
        env "THEME_FONT_FAMILY" ++ ":medium:size=" ++ env "THEME_FONT_SIZE"
    , Tabbed.activeColor = env "THEME_BLACK"
    , Tabbed.activeBorderColor = env "THEME_BLACK"
    , Tabbed.activeTextColor = env "THEME_LIGHT_CYAN"
    , Tabbed.inactiveColor = env "THEME_DARK_GRAY"
    , Tabbed.inactiveBorderColor = env "THEME_DARK_GRAY"
    , Tabbed.inactiveTextColor = env "THEME_LIGHT_GREEN"
    }

layout =
  NoBorders.smartBorders $
  tall ||| Layout.Mirror tall ||| Tabbed.tabbed Tabbed.shrinkText tabTheme
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
  [ ((mod4Mask .|. shiftMask, xK_Return), Core.spawn $ Core.terminal conf) -- %! Launch terminal
  , ((mod4Mask .|. mod1Mask, xK_l), toggleScratchpad "qutebrowser")
  , ((mod4Mask .|. mod1Mask, xK_w), toggleScratchpad "keepassx2")
  -- Launch dmenu
  , ((mod4Mask, xK_minus), Core.spawn "dmenu_run")
  -- Launch dmenu
  , ((mod4Mask, xK_v), Core.spawn "clipmenu")
  -- Close the focused window
  , ((mod4Mask .|. shiftMask, xK_c), Operations.kill)
  -- Close the focused window
  , ((mod4Mask .|. shiftMask, xK_s), toggleScratchpad "top")
  -- Rotate through the available layout algorithms
  , ((mod4Mask, xK_space), Operations.sendMessage Layout.NextLayout)
  -- Reset the layouts on the current workspace to default
  , ( (mod4Mask .|. shiftMask, xK_space)
    , Operations.setLayout $ Core.layoutHook conf)
  -- Resize viewed Operations.windows to the correct size
  , ((mod4Mask, xK_n), toggleScratchpad "mid")
  -- Move focus to the next window
  , ((mod4Mask, xK_Tab), Operations.windows StackSet.focusDown)
  -- Move focus to the previous window
  , ((mod4Mask .|. shiftMask, xK_Tab), Operations.windows StackSet.focusUp)
  -- Move focus to the next window
  , ((mod4Mask, xK_j), Operations.windows StackSet.focusDown)
  -- Move focus to the previous window
  , ((mod4Mask, xK_k), Operations.windows StackSet.focusUp)
  -- Move focus to the master window
  , ((mod4Mask, xK_m), Operations.windows StackSet.focusMaster)
  -- Move focus to the master window
  , ((mod4Mask, xK_s), toggleScratchpad "bottom")
  -- Swap the focused window and the master window
  , ((mod4Mask, xK_Return), Operations.windows StackSet.swapMaster)
  -- Swap the focused window with the next window
  , ((mod4Mask .|. shiftMask, xK_j), Operations.windows StackSet.swapDown)
  -- Swap the focused window with the previous window
  , ((mod4Mask .|. shiftMask, xK_k), Operations.windows StackSet.swapUp)
  -- Shrink the master area
  , ((mod4Mask, xK_h), Operations.sendMessage Layout.Shrink)
  -- Expand the master area
  , ((mod4Mask, xK_l), Operations.sendMessage Layout.Expand)
  -- Push window back into tiling
  , ( (mod4Mask, xK_t)
    , Operations.withFocused $ Operations.windows . StackSet.sink)
  -- Increment the number of Operations.windows in the master area
  , ((mod4Mask, xK_comma), Operations.sendMessage (Layout.IncMasterN 1))
  -- Deincrement the number of windows in the master area
  , ((mod4Mask, xK_period), Operations.sendMessage (Layout.IncMasterN (-1)))
  -- Quit XMonad
  , ((mod4Mask .|. shiftMask, xK_q), Core.io (Exit.exitWith Exit.ExitSuccess))
  -- Restart XMonad
  , ((mod4Mask, xK_q), Core.spawn "xmonad --recompile && xmonad --restart")
  ] ++
  -- project workspaces
  [ ( (mod4Mask .|. mod1Mask, key)
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
  [ ((mod4Mask, key), Operations.windows $ StackSet.greedyView workspace)
  | (workspace, key) <- zip (Core.workspaces conf) [xK_1 .. xK_9]
  ] ++
  -- mod-shift-[1..9]: Move client to workspace N
  [ ( (mod4Mask .|. shiftMask, key)
    , Operations.windows $ StackSet.shift workspace)
  | (workspace, key) <- zip (Core.workspaces conf) [xK_1 .. xK_9]
  ] ++
  -- mod-{w,e,r}: Switch to physical/Xinerama screens 1, 2, or 3
  [ ( (mod4Mask, key)
    , Operations.screenWorkspace sc >>=
      flip Core.whenJust (Operations.windows . StackSet.view))
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
  ] ++
  -- mod-shift-{w,e,r}: Move client to screen 1, 2, or 3
  [ ( (mod4Mask .|. shiftMask, key)
    , Operations.screenWorkspace sc >>=
      flip Core.whenJust (Operations.windows . StackSet.shift))
  | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
  ]

myScratchpads =
  [ NamedScratchpad.NS
      "qutebrowser"
      "qutebrowser"
      (ManageHook.appName =? "qutebrowser")
      (centeredLayout defaultMargin)
  , NamedScratchpad.NS
      "mid"
      "ttymux mid"
      (ManageHook.appName =? "mid")
      (centeredLayout 0)
  , NamedScratchpad.NS
      "top"
      "ttymux top"
      (ManageHook.appName =? "top")
      (topLayout defaultMargin)
  , NamedScratchpad.NS
      "bottom"
      "ttymux bottom"
      (ManageHook.appName =? "bottom")
      (bottomLayout defaultMargin)
  , NamedScratchpad.NS
      "keepassx2"
      "keepassx2"
      (ManageHook.appName =? "keepassx2")
      (centeredLayout 0.35)
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
