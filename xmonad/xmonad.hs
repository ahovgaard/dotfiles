--
-- ~/.xmonad/xmonad.hs
--

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO

main :: IO ()
main = do
  dzenLeftBar  <- spawnPipe callDzen1 -- logHook statusbar
  spawn callDzen2                     -- conky piped statusbar
  xmonad $ defaultConfig
    { manageHook         = manageDocks <+> myManageHook
    , layoutHook         = smartBorders $ avoidStruts $ layoutHook defaultConfig
    , logHook            = myLogHook dzenLeftBar
    , terminal           = "urxvtc"
    , modMask            = mod4Mask
    , borderWidth        = 2 
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#ff0000"
    , startupHook        = setWMName "LG3D"
    } `additionalKeys` myKeys
       where callDzen1 = "dzen2 -ta l -fn '"
                         ++ dzenFont
                         ++ "' -bg '#000000' -w 500 -h 18"
             callDzen2 = "conky ~/.conkyrc | dzen2 -x 500 -ta r -fn '"
                         ++ dzenFont
                         ++ "' -bg '#000000' -h 18"
             dzenFont  = "Inconsolata-8"

-- Float uzbl's bookmarking dialog box windows
myManageHook = composeAll [ title =? "Add a new entry" --> doFloat
                          , title =? "xfce4-notifyd"   --> doIgnore
                          ]

-------------------------------------------------------------------------------
-- Configuration of dzen2 status bar
-------------------------------------------------------------------------------
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
  { ppCurrent = dzenColor "#ffffff" "#555555" . pad
  , ppUrgent  = dzenColor "#ff0000" "#000000" . pad
  , ppWsSep   = " "
  , ppSep     = "  |  "
  , ppLayout = dzenColor "#ffffff" "#000000" .
      (\x -> case x of
        "Tall"        -> tileIcon
        "Full"        -> fullIcon
        "Mirror Tall" -> mirrorIcon
        _ -> x
      )
  , ppTitle   = (" " ++) . dzenColor "#ffffff" "#000000" . dzenEscape
  , ppOutput  = hPutStrLn h
  } where iconDir    = "/home/akh/.xmonad/dzen/icons/"
          icon s     = "^i(" ++ s ++ ")^ca()"
          tileIcon   = icon $ iconDir ++ "stlarch/tile.xbm"
          fullIcon   = icon $ iconDir ++ "stlarch/monocle.xbm"
          mirrorIcon = icon $ iconDir ++ "stlarch/bstack.xbm"

-------------------------------------------------------------------------------
-- Keybindings
-------------------------------------------------------------------------------
myKeys = [ -- Lock screen
           ((mod4Mask .|. shiftMask, xK_z), spawn "slock & systemctl suspend")

           -- Toggle touchpad on/off, hide cursor
         , ((mod4Mask .|. shiftMask, xK_x), spawn "toggleTouchpad.sh")

           -- Toggle external HP monitor
         , ((0, xF86XK_Display), spawn "toggleHpMonitor.sh")

           -- Toggle gaps for dock/panel/trayer
         , ((mod4Mask .|. shiftMask, xK_b), sendMessage ToggleStruts)

           -- Applications
         , ((mod4Mask .|. shiftMask, xK_p), spawn "gmrun")
         , ((mod4Mask, xK_b), spawn "uzbl-tabbed")
         , ((mod4Mask, xK_f), spawn "urxvtc -e ranger")
         , ((mod4Mask, xK_a), spawn "urxvtc -e mutt")

           -- Multimedia
         , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 2+")
         , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 2-")
         , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")
         , ((0, xF86XK_AudioNext), spawn "ncmpcpp next")
         , ((0, xF86XK_AudioPrev), spawn "ncmpcpp prev")
         , ((0, xF86XK_AudioPlay), spawn "ncmpcpp toggle")

           -- Screenshot
         , ((0, xK_Print), spawn "scrot")
         ]
