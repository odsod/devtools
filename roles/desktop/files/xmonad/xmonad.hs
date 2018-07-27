-- Needed for Layout
{-# LANGUAGE FlexibleContexts #-}

-- XMonad Core
import qualified XMonad.Config                  as Config
import qualified XMonad.Core                    as Core
import qualified XMonad.Layout                  as Layout
import qualified XMonad.Main                    as Main
import qualified XMonad.ManageHook              as ManageHook
import qualified XMonad.Operations              as Operations

-- XMonad Contrib
import qualified XMonad.Actions.DynamicProjects as DynamicProjects
import qualified XMonad.Config.Kde              as Kde
import qualified XMonad.Hooks.SetWMName         as SetWMName
import qualified XMonad.Layout.NoBorders        as NoBorders
import qualified XMonad.StackSet                as StackSet
import qualified XMonad.Util.NamedScratchpad    as NamedScratchpad

-- X11 keyboard symbols
import           Graphics.X11

-- Haskell
import qualified Control.Monad                  as Monad
import qualified Data.Char                      as Char
import qualified Data.List                      as List
import qualified Data.Map                       as Map
import qualified Data.Monoid                    as Monoid
import qualified System.Exit                    as Exit

-- Operators
import           Data.Bits                      ((.|.))
import           XMonad.Layout                  ((|||))
import           XMonad.ManageHook              ((-->), (=?))

-- | Assume Tab has been remapped to Hyper with xcape
tabMask :: KeyMask
tabMask = mod3Mask

-- | Assume Mod1 is mapped to Alt
altMask :: KeyMask
altMask = mod1Mask

-- | Assume Mod5 is mapped to AltGr
altGrMask :: KeyMask
altGrMask = mod5Mask

workspaces = ["7", "8", "9", "0"] ++ projectWorkspaces
  where
    projects = map snd keysAndProjects
    projectWorkspaces = map DynamicProjects.projectName projects

startupHook = do
  Core.startupHook Kde.kde4Config
  SetWMName.setWMName "LG3D"

manageHook =
  Monoid.mconcat
    [ Core.manageHook Kde.kde4Config
    , wmQuery "plasma-desktop" --> ManageHook.doFloat
    , wmQuery "plasmashell" --> ManageHook.doFloat
    , NamedScratchpad.namedScratchpadManageHook scratchpads
    ]
  where
    scratchpads = map snd keysAndScratchpads

layoutHook =
  Kde.desktopLayoutModifiers $
  NoBorders.smartBorders (tall ||| Layout.Mirror tall ||| Layout.Full)
  where
    tall =
      Layout.Tall
        { Layout.tallNMaster = 1
        , Layout.tallRatioIncrement = 3 / 100
        , Layout.tallRatio = 1 / 2
        }

keysAndProjects :: [((KeyMask, KeySym), DynamicProjects.Project)]
keysAndProjects =
  [ ((tabMask .|. altMask, xK_h), project "goland")
  , ((tabMask .|. altMask, xK_m), project "evince")
  , ((tabMask .|. altMask, xK_p), project "spotify")
  , ((tabMask .|. altMask, xK_g), chromeProject "mail.google.com")
  , ((tabMask .|. altMask, xK_c), chromeProject "calendar.google.com")
  , ((tabMask .|. altMask, xK_r), chromeProject "drive.google.com")
  , ((tabMask .|. altMask, xK_l), chromeProject "meet.google.com")
  , ((tabMask .|. altMask, xK_s), chromeProject "einride.slack.com")
  ]
  where
    project cmd =
      DynamicProjects.Project
        { DynamicProjects.projectName = cmd
        , DynamicProjects.projectDirectory = "~/"
        , DynamicProjects.projectStartHook = Just $ Core.spawn cmd
        }
    chromeProject url =
      DynamicProjects.Project
        { DynamicProjects.projectName = url
        , DynamicProjects.projectDirectory = "~/"
        , DynamicProjects.projectStartHook =
            Just $ Core.spawn $ "google-chrome --app=\"https://" ++ url ++ "\""
        }

keysAndScratchpads :: [((KeyMask, KeySym), NamedScratchpad.NamedScratchpad)]
keysAndScratchpads =
  [ ((tabMask, xK_u), midTerminal)
  , ((tabMask, xK_h), leftTerminal)
  , ((tabMask, xK_t), rightTerminal)
  , ((tabMask, xK_n), topTerminal)
  , ((tabMask, xK_s), bottomTerminal)
  , ((tabMask, xK_e), dolphin)
  , ((tabMask, xK_c), systemsettings)
  , ((tabMask, xK_p), spectacle)
  , ((tabMask .|. altGrMask, xK_u), keepassxc)
  , ((tabMask .|. altGrMask, xK_h), googleChrome)
  ]
  where
    floating = NamedScratchpad.customFloating
    widescreen margin = margin * (10 / 16)
    opposite margin = 1 - 2 * margin
    rect = StackSet.RationalRect
    rectWithMargin margin =
      StackSet.RationalRect
        (widescreen margin)
        margin
        (opposite $ widescreen margin)
        (opposite margin)
    -- Scratchpads
    midTerminal =
      NamedScratchpad.NS
        "mid"
        "urxvtmux mid"
        (wmQuery "mid")
        (floating $ rect 0 0 1 1)
    topTerminal =
      NamedScratchpad.NS
        "top"
        "urxvtmux top"
        (wmQuery "top")
        (floating $ rect 0 0 1 0.5)
    bottomTerminal =
      NamedScratchpad.NS
        "bottom"
        "urxvtmux bottom"
        (wmQuery "bottom")
        (floating $ rect 0 0.5 1 0.5)
    leftTerminal =
      NamedScratchpad.NS
        "left"
        "urxvtmux left"
        (wmQuery "left")
        (floating $ rect 0 0 0.5 1)
    rightTerminal =
      NamedScratchpad.NS
        "right"
        "urxvtmux right"
        (wmQuery "right")
        (floating $ rect 0.5 0 0.5 1)
    dolphin =
      NamedScratchpad.NS
        "dolphin"
        "dolphin"
        (wmQuery "dolphin")
        (floating $ rectWithMargin 0.05)
    systemsettings =
      NamedScratchpad.NS
        "systemsettings5"
        "systemsettings5"
        (wmQuery "systemsettings5")
        (floating $ rectWithMargin 0.05)
    keepassxc =
      NamedScratchpad.NS
        "keepassxc"
        "keepassxc"
        (wmQuery "keepassxc")
        (floating $ rectWithMargin 0.20)
    spectacle =
      NamedScratchpad.NS
        "spectacle"
        "spectacle"
        (wmQuery "spectacle")
        (floating $ rectWithMargin 0.3)
    googleChrome =
      NamedScratchpad.NS
        "google-chrome"
        "google-chrome --user-data-dir=.config/google-chrome-scratchpad"
        (wmQuery "google-chrome (.config/google-chrome-scratchpad)")
        (floating $ rectWithMargin 0.035)

-- | Convenience query for WM properties
wmQuery :: String -> Core.Query Bool
wmQuery s =
  (caseInsensitive ManageHook.className =? s) `or`
  (caseInsensitive ManageHook.appName =? s)
  where
    caseInsensitive = (fmap . fmap) Char.toLower
    or = Monad.liftM2 (||)

viewWorkspace :: String -> Core.X ()
viewWorkspace s = do
  Operations.windows $ StackSet.greedyView s
  Core.spawn ("notify-send --expire-time=300 " ++ s)

keys conf =
  Map.fromList
    (layoutKeys ++
     projectKeys ++ scratchpadKeys ++ screenKeys ++ workspaceKeys ++ otherKeys)
  where
    projectKeys =
      [ (maskAndKey, viewWorkspace $ DynamicProjects.projectName project)
      | (maskAndKey, project) <- keysAndProjects
      ]
    scratchpadKeys =
      [ (maskAndKey, toggleScratchpad (NamedScratchpad.name scratchpad))
      | (maskAndKey, scratchpad) <- keysAndScratchpads
      ]
    workspaceKeys =
      [ ((mod3Mask, xK_7), viewWorkspace "7")
      , ((mod3Mask .|. shiftMask, xK_7), moveWindowToWorkspace "7")
      , ((mod3Mask, xK_8), viewWorkspace "8")
      , ((mod3Mask .|. shiftMask, xK_8), moveWindowToWorkspace "8")
      , ((mod3Mask, xK_9), viewWorkspace "9")
      , ((mod3Mask .|. shiftMask, xK_9), moveWindowToWorkspace "9")
      , ((mod3Mask, xK_0), viewWorkspace "0")
      , ((mod3Mask .|. shiftMask, xK_0), moveWindowToWorkspace "0")
      ]
    screenKeys =
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
    layoutKeys =
      [ ((mod3Mask, xK_space), Operations.sendMessage Layout.NextLayout)
      , ((mod3Mask, xK_Return), Operations.windows StackSet.swapMaster)
      , ((mod3Mask, xK_j), Operations.windows StackSet.focusDown)
      , ((mod3Mask, xK_k), Operations.windows StackSet.focusUp)
      , ((mod3Mask, xK_comma), Operations.sendMessage (Layout.IncMasterN 1))
      , ((mod3Mask, xK_period), Operations.sendMessage (Layout.IncMasterN (-1)))
      , ((mod3Mask .|. shiftMask, xK_u), Operations.sendMessage Layout.Shrink)
      , ((mod3Mask .|. shiftMask, xK_h), Operations.sendMessage Layout.Expand)
      , ((mod3Mask .|. shiftMask, xK_t), Operations.windows StackSet.swapDown)
      , ((mod3Mask .|. shiftMask, xK_n), Operations.windows StackSet.swapUp)
      ]
    otherKeys =
      [ ((mod1Mask .|. controlMask, xK_semicolon), Core.spawn "init-keyboard")
      , ((mod3Mask, xK_r), Core.spawn "rofi -show run")
      , ((mod3Mask .|. shiftMask, xK_Return), Core.spawn $ Core.terminal conf)
      , ((mod3Mask .|. shiftMask, xK_space), resetLayout)
      , ((mod3Mask .|. shiftMask, xK_c), Operations.kill)
      , ((mod3Mask .|. shiftMask, xK_s), sinkWindow)
      ]
    sinkWindow = Operations.withFocused $ Operations.windows . StackSet.sink
    resetLayout = Operations.setLayout $ Core.layoutHook conf
    scratchpads = map snd keysAndScratchpads
    toggleScratchpad = NamedScratchpad.namedScratchpadAction scratchpads
    moveWindowToWorkspace = Operations.windows . StackSet.shift
    withScreen n op = Operations.screenWorkspace n >>= flip Core.whenJust op

main :: IO ()
main =
  Main.xmonad $
  DynamicProjects.dynamicProjects projects $
  Kde.kde4Config
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
  where
    projects = map snd keysAndProjects
