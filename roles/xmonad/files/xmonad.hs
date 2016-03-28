{-# LANGUAGE NoMonomorphismRestriction #-} -- needed for tabs

import XMonad
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.StackSet hiding (workspaces)
import XMonad.Layout.Tabbed

import Data.List
import System.Environment
import System.IO.Unsafe

myWorkspaces = [("h", xK_h), ("c", xK_c), ("r", xK_r), ("l", xK_l)]

env = unsafePerformIO . getEnv

main = xmonad $ defaultConfig {
  modMask            = mod4Mask
, terminal           = "urxvtc"
, workspaces         = map fst myWorkspaces
, startupHook        = windows $ greedyView $ fst $ head myWorkspaces
, focusedBorderColor = env "THEME_LIGHT_GREEN"
, normalBorderColor  = env "THEME_BLACK"
, layoutHook         = myLayout
, focusFollowsMouse  = False
, manageHook         = namedScratchpadManageHook myScratchpads
} `additionalKeys` myKeys

tabTheme = defaultTheme {
  fontName = "xft:PragmataPro:medium:size=9"
, activeColor = env "THEME_BLACK"
, activeBorderColor = env "THEME_BLACK"
, activeTextColor = env "THEME_LIGHT_CYAN"
, inactiveColor = env "THEME_DARK_GRAY"
, inactiveBorderColor = env "THEME_DARK_GRAY"
, inactiveTextColor = env "THEME_LIGHT_GREEN"
}

myLayout = smartBorders $ tall ||| Mirror tall ||| tabs
  where tall = Tall 1 (3/100) (1/2) -- documented defaults
        tabs = tabbed shrinkText tabTheme

toggleScratchpad = namedScratchpadAction myScratchpads

myKeys = modKeys ++ modShiftKeys ++ scratchpadKeys ++ switchWorkspaceKeys where

  modKeys = [((mod4Mask, key), action) | (key, action) <- binds]
    where binds = [ (xK_l, sendMessage Expand)
                  , (xK_h, sendMessage Shrink)
                  , (xK_n, toggleScratchpad "mid")
                  , (xK_s, toggleScratchpad "bottom")
                  , (xK_minus, spawn "mydmenu")
                  , (xK_q, spawn "xmonad --recompile; xmonad --restart")
                  , (xK_j, windows focusDown)
                  , (xK_k, windows focusUp)
                  , (xK_v, spawn "clipmenu")
                  , (xK_Return, windows swapMaster)
                  ]

  scratchpadKeys = [((mod4Mask .|. mod1Mask, key), toggleScratchpad name) | (key, name) <- binds]
    where binds = [ (xK_c, "calendar")
                  , (xK_d, "drive")
                  , (xK_l, "dwb")
                  , (xK_b, "chromium")
                  , (xK_t, "ipython")
                  , (xK_s, "slack")
                  , (xK_m, "mail")
                  , (xK_w, "keepassx")
                  , (xK_z, "zeal")
                  ]

  modShiftKeys = [((mod4Mask .|. shiftMask, key), action) | (key, action) <- binds]
    where binds = [ (xK_c, kill)
                  , (xK_l, sendMessage NextLayout)
                  , (xK_s, toggleScratchpad "top")
                  ]

  switchWorkspaceKeys = [((modKey .|. mod4Mask .|. mod5Mask, k), windows $ f i)
      | (i, k) <- myWorkspaces
      , (f, modKey) <- [(greedyView, 0), (shift, shiftMask)]]

myScratchpads =
  [ NS "dwb" "dwb"
       (appName =? "dwb") (centeredLayout defaultMargin)
  , NS "chromium" "chromium-browser"
       (appName =? "chromium-browser") (centeredLayout defaultMargin)
  , NS "mid" "ttymux mid"
       (appName =? "mid") (centeredLayout 0)
  , NS "top" "ttymux top"
       (appName =? "top") (topLayout defaultMargin)
  , NS "bottom" "ttymux bottom"
       (appName =? "bottom") (bottomLayout defaultMargin)
  , NS "keepassx" "keepassx"
       (appName =? "keepassx") (centeredLayout 0.35)
  , NS "zeal" "zeal"
       (appName =? "zeal") (centeredLayout 0.15)
  , NS "mail" "chromium-browser --app=http://mail.spotify.com"
       (appName =? "mail.spotify.com") (centeredLayout defaultMargin)
  , NS "calendar" "chromium-browser --app=https://calendar.google.com"
       (appName =? "calendar.google.com") (centeredLayout defaultMargin)
  , NS "slack" "chromium-browser --app=https://spotify.slack.com"
       (appName =? "spotify.slack.com") (centeredLayout defaultMargin)
  , NS "drive" "chromium-browser --app=https://drive.google.com"
       (appName =? "drive.google.com") (centeredLayout defaultMargin)
  ]
  where defaultMargin = 0.035
        widescreen margin = margin * 10 / 16
        opposite margin = 1 - 2 * margin
        centeredLayout margin = customFloating $ RationalRect
          (widescreen margin) margin (opposite $ widescreen margin) (opposite margin)
        topLayout margin = customFloating $ RationalRect
          (widescreen margin) margin (opposite $ widescreen margin) (0.5 - 1.5 * margin)
        bottomLayout margin = customFloating $ RationalRect
          (widescreen margin) (0.5 + 0.5 * margin) (opposite $ widescreen margin) (0.5 - 1.5 * margin)
