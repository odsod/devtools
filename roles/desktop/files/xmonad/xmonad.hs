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
import qualified XMonad.Config.Gnome         as Gnome
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
  Gnome.gnomeConfig
    { Core.modMask = mod3Mask
    , Core.terminal = "urxvtc"
    , Core.keys = keys
    , Core.layoutHook = layoutHook
    , Core.focusFollowsMouse = False
    , Core.normalBorderColor = "#073642"
    , Core.focusedBorderColor = "#586e75"
    , Core.manageHook = manageHook
    , Core.workspaces = workspaces
    , Core.startupHook = startupHook
    }

workspaces =
  ["7", "8", "9", "0"] ++ ["google-chrome", "mail", "calendar", "drive"]

startupHook = do
  Core.startupHook Gnome.gnomeConfig
  SetWMName.setWMName "LG3D"

manageHook =
  Monoid.mconcat
    [ Core.manageHook Gnome.gnomeConfig
    , NamedScratchpad.namedScratchpadManageHook scratchpads
    , className "google-chrome" --> ManageHook.doShift "google-chrome"
    ]
  where
    className c = (fmap . fmap) Char.toLower ManageHook.className =? c

layoutHook =
  Gnome.desktopLayoutModifiers $
  NoBorders.smartBorders (tall ||| Layout.Mirror tall)
  where
    tall =
      Layout.Tall
        { Layout.tallNMaster = 1
        , Layout.tallRatioIncrement = 3 / 100
        , Layout.tallRatio = 1 / 2
        }

scratchpads =
  [ terminalScratchpad "mid" 0 0 1 1
  , terminalScratchpad "top" 0 0 1 0.5
  , terminalScratchpad "bottom" 0 0.5 1 0.5
  , terminalScratchpad "left" 0 0 0.5 1
  , terminalScratchpad "right" 0.5 0 0.5 1
  , NamedScratchpad.NS
      "keepassxc"
      "keepassxc"
      (ManageHook.appName =? "keepassxc")
      (centeredLayout 0.20)
  , NamedScratchpad.NS
      "gnome-screenshot"
      "gnome-screenshot --interactive"
      (ManageHook.appName =? "gnome-screenshot")
      (centeredLayout 0.30)
  , NamedScratchpad.NS
      "gnome-control-center"
      "gnome-control-center"
      (className "gnome-control-center")
      (centeredLayout 0.05)
  , NamedScratchpad.NS
      "google-chrome"
      "google-chrome --user-data-dir=.config/google-chrome-scratchpad"
      (ManageHook.appName =? "google-chrome (.config/google-chrome-scratchpad)")
      (centeredLayout 0.035)
  ]
  where
    widescreen margin = margin * (10 / 16)
    opposite margin = 1 - 2 * margin
    centeredLayout margin =
      NamedScratchpad.customFloating $
      StackSet.RationalRect
        (widescreen margin)
        margin
        (opposite $ widescreen margin)
        (opposite margin)
    terminalScratchpad name left top right bottom =
      NamedScratchpad.NS
        name
        ("urxvtmux " ++ name)
        (ManageHook.appName =? name)
        (NamedScratchpad.customFloating $
         StackSet.RationalRect left top right bottom)
    className c = (fmap . fmap) Char.toLower ManageHook.className =? c

keys conf =
  Map.fromList $
  [ ((mod3Mask .|. shiftMask, xK_Return), Core.spawn $ Core.terminal conf)
  , ((mod1Mask .|. controlMask, xK_semicolon), Core.spawn "init-keyboard")
  , ((mod3Mask, xK_minus), Gnome.gnomeRun)
  , ((mod3Mask, xK_space), Operations.sendMessage Layout.NextLayout)
  , ((mod3Mask .|. shiftMask, xK_space), resetLayout)
  , ((mod3Mask, xK_c), toggleScratchpad "gnome-control-center")
  , ((mod3Mask .|. shiftMask, xK_c), Operations.kill)
  , ((mod3Mask, xK_p), toggleScratchpad "gnome-screenshot")
  , ((mod3Mask, xK_n), toggleScratchpad "mid")
  , ((mod3Mask, xK_s), toggleScratchpad "top")
  , ((mod3Mask .|. mod1Mask, xK_s), toggleScratchpad "bottom")
  , ((mod3Mask, xK_t), toggleScratchpad "left")
  , ((mod3Mask .|. mod1Mask, xK_t), toggleScratchpad "right")
  , ((mod3Mask .|. mod1Mask, xK_l), toggleScratchpad "google-chrome")
  , ((mod3Mask .|. mod1Mask, xK_w), toggleScratchpad "keepassxc")
  , ((mod3Mask, xK_j), Operations.windows StackSet.focusDown)
  , ((mod3Mask, xK_k), Operations.windows StackSet.focusUp)
  , ((mod3Mask, xK_Return), Operations.windows StackSet.swapMaster)
  , ((mod3Mask .|. shiftMask, xK_j), Operations.windows StackSet.swapDown)
  , ((mod3Mask .|. shiftMask, xK_k), Operations.windows StackSet.swapUp)
  , ((mod3Mask, xK_h), Operations.sendMessage Layout.Shrink)
  , ((mod3Mask, xK_l), Operations.sendMessage Layout.Expand)
    {-, ((mod3Mask, xK_t), sinkWindow)-}
  , ((mod3Mask, xK_comma), Operations.sendMessage (Layout.IncMasterN 1))
  , ((mod3Mask, xK_period), Operations.sendMessage (Layout.IncMasterN (-1)))
  , ((mod3Mask .|. mod1Mask, xK_m), viewWorkspace "mail")
  , ((mod3Mask .|. mod1Mask, xK_c), viewWorkspace "calendar")
  , ((mod3Mask .|. mod1Mask, xK_d), viewWorkspace "drive")
  , ((mod3Mask .|. mod1Mask, xK_b), viewWorkspace "google-chrome")
  , ((mod3Mask, xK_7), viewWorkspace "7")
  , ((mod3Mask .|. shiftMask, xK_7), moveWindowToWorkspace "7")
  , ((mod3Mask, xK_8), viewWorkspace "8")
  , ((mod3Mask .|. shiftMask, xK_8), moveWindowToWorkspace "8")
  , ((mod3Mask, xK_9), viewWorkspace "9")
  , ((mod3Mask .|. shiftMask, xK_9), moveWindowToWorkspace "9")
  , ((mod3Mask, xK_0), viewWorkspace "0")
  , ((mod3Mask .|. shiftMask, xK_0), moveWindowToWorkspace "0")
    {-, ((mod3Mask, xK_1), withScreen 0 viewWorkspace)-}
    {-, ((mod3Mask .|. shiftMask, xK_1), withScreen 0 moveWindowToWorkspace)-}
    {-, ((mod3Mask, xK_2), withScreen 1 viewWorkspace)-}
    {-, ((mod3Mask .|. shiftMask, xK_2), withScreen 1 moveWindowToWorkspace)-}
  ] ++
  [ ( (mod3Mask, key)
    , Operations.screenWorkspace sc >>=
      flip Core.whenJust (Operations.windows . StackSet.view))
  | (key, sc) <- zip [xK_1, xK_2] [0 ..]
  ] ++
  [ ( (mod3Mask .|. shiftMask, key)
    , Operations.screenWorkspace sc >>=
      flip Core.whenJust (Operations.windows . StackSet.shift))
  | (key, sc) <- zip [xK_1, xK_2] [0 ..]
  ]
  where
    viewWorkspace = Operations.windows . StackSet.greedyView
    moveWindowToWorkspace = Operations.windows . StackSet.shift
    withScreen n op = Operations.screenWorkspace n >>= flip Core.whenJust op
    sinkWindow = Operations.withFocused $ Operations.windows . StackSet.sink
    resetLayout = Operations.setLayout $ Core.layoutHook conf
    toggleScratchpad = NamedScratchpad.namedScratchpadAction scratchpads
