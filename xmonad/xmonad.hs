--
-- ~/.xmonad/xmonad.hs
--

{-# LANGUAGE NoMonomorphismRestriction #-}

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.GridSelect
-- import XMonad.Actions.HelloWorld
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad

import Data.List (isInfixOf)
import qualified Data.Map as Map
import Graphics.X11.ExtraTypes.XF86
import System.IO


main :: IO ()
main = do
  dzenLeftBar  <- spawnPipe xmonadBar -- logHook statusbar
  spawn "/home/akh/.xmonad/scripts/dzen_trayer.sh"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { normalBorderColor  = colorBlack
    , focusedBorderColor = colorRed
    , terminal           = myTerminal

    -- the available layouts
    , layoutHook         = myLayoutHook

    -- the action to run when a new window is opened
    , manageHook         = manageDocks <+> myManageHook

    , workspaces         = myWorkspaces
    , modMask            = myModMask
    , borderWidth        = 2

    -- might fix problem with Java applications and non-reparenting WMs
    , startupHook = setWMName "LG3D"

    -- the action to perform when the window set is changed
    , logHook            = myLogHook dzenLeftBar
    } `additionalKeys` myKeys `additionalKeysP` myKeys2
       where xmonadBar = "dzen2 -ta l -fn '"
                         ++ barFont
                         ++ "' -bg '#000000' -w 600 -h 16"


myKeys2 = [ -- lock screen
            ("M-S-z",   spawn "systemctl suspend & slock")

            -- toggle touchpad and hide cursor
          , ("M-S-x",   spawn "toggleTouchpad.sh")

            -- toggle gaps for dock/panel/trayer
          , ("M-S-b",   sendMessage ToggleStruts)

            -- applications
          , ("M-S-p",   spawn "gmrun")
          , ("M-n",     namedScratchpadAction scratchpads "newsbeuter")
          , ("M-f",     spawn "urxvtc -e ranger")
          , ("M-a",     spawn "urxvtc -e mutt")

            -- volume control
          , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 2%+ unmute")
          , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 2%- unmute")
          , ("<XF86AudioMute>",        spawn "amixer -q sset Master toggle")

            -- prompts
          , ("M-p p",   shellPrompt myXPConfig)
          , ("M-/",     windowPromptGoto myXPConfig)

            -- add a line to notes file from a prompt
          , ("M-p n",   spawn ("echo -n '\n  * ' >> /home/akh/TODO.md") >>
                        appendFilePrompt defaultXPConfig "/home/akh/TODO.md")
          ]


-------------------------------------------------------------------------------
-- Misc. useful bindings
-------------------------------------------------------------------------------
myTerminal = "urxvtc"
myModMask  = mod4Mask

-------------------------------------------------------------------------------
-- Colors, fonts and prompts
-------------------------------------------------------------------------------
colorBlack = "#000000"
colorRed   = "#ff0000"
colorWhite = "#ffffff"
colorGray  = "#555555"

barFont = "-*-terminus-*-r-normal-*-*-80-*-*-*-*-iso8859-*"

myTabTheme =
  defaultTheme { fontName            = barFont
               --, inactiveBorderColor = colorGrayAlt
               --, inactiveColor       = colorDarkGray
               --, inactiveTextColor   = colorGrayAlt
               --, activeBorderColor   = colorGrayAlt
               --, activeColor         = colorDarkMagenta
               --, activeTextColor     = colorDarkGray
               --, urgentBorderColor   = colorBlackAlt
               --, urgentTextColor     = colorWhite
               , decoHeight          = 14
               }


myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { searchPredicate = \query compl -> all (`isInfixOf` compl) (words query)
  , alwaysHighlight = True
  }

-------------------------------------------------------------------------------
-- Manage hook, scratchpad
-------------------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = ["work", "www", "docs", "mail", "music",
                "movie", "other", "irc", "sys"]

myManageHook :: ManageHook
myManageHook =  manageDocks
            <+> manageScratchPad
            <+> namedScratchpadManageHook scratchpads
            <+> composeAll [ title     =? "Add a new entry" --> doFloat
                           , title     =? "xfce4-notifyd"   --> doIgnore
                           , className =? "MPlayer"         --> doFloat
                           , title     =? "mutt"            --> doShift "mail"
                           , isFullscreen                   --> doFullFloat
                           ]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where h = 0.6   -- terminal height
        w = 0.6   -- terminal width
        t = 0.2   -- distance from top edge
        l = 0.2   -- distance from left edge


-------------------------------------------------------------------------------
-- Layout hook
-------------------------------------------------------------------------------
-- Automatically avoid struts on no sides of the screen ([]),
-- i.e. don't avoid struts by default.
--myLayoutHook = smartBorders $ avoidStrutsOn [] $ layoutHook defaultConfig
--myLayoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
myLayoutHook = smartBorders $ avoidStruts $
                (ResizableTall 1 (3/100) (1/2) [])
            ||| simpleTabbed
            ||| Full
--            ||| myMain
--  where myMain = layoutN 1 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) Full
--               $ layoutN 1 (relBox 0.5 0 1 0.5) (Just $ relBox 0.5 0 1 1) Full
--               $ layoutAll (relBox 0.5 0.5 1 1) simpleTabbed


-------------------------------------------------------------------------------
-- Configuration of dzen2 status bar
-------------------------------------------------------------------------------
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
  { ppCurrent = dzenColor colorWhite colorGray  . pad
  , ppUrgent  = dzenColor colorRed   colorBlack . pad
  , ppWsSep   = " "
  , ppSep     = "  |  "
  , ppLayout  = dzenColor colorWhite colorBlack
                  .  (\x -> case x of "Tall"          -> tileIcon
                                      "Full"          -> fullIcon
                                      "Mirror Tall"   -> mirrorIcon
                                      "ResizableTall" -> tileIcon
                                      _               -> x)
  , ppTitle   = dzenColor colorWhite colorBlack . dzenEscape . take 50
  , ppHidden  = noScratchPad
  , ppOutput  = hPutStrLn h
  } where iconDir    = "/home/akh/.xmonad/dzen/icons/"
          icon s     = "^i(" ++ s ++ ")^ca()"
          tileIcon   = icon $ iconDir ++ "stlarch/tile.xbm"
          fullIcon   = icon $ iconDir ++ "stlarch/monocle.xbm"
          mirrorIcon = icon $ iconDir ++ "stlarch/bstack.xbm"

          noScratchPad ws = if ws == "NSP" then "" else ws


-------------------------------------------------------------------------------
-- Keybindings
-------------------------------------------------------------------------------
myKeys = [ -- Toggle external HP monitor
           ((0, xF86XK_Display), spawn "toggleHpMonitor.sh")

           -- GridSelect
         --, ((mod4Mask,               xK_g), goToSelected myGSConfig)
         --, ((mod4Mask .|. shiftMask, xK_g), bringSelected myGSConfig)

           -- Scratchpad
         , ((mod4Mask, xK_o), scratchPad)


           -- Screenshot
         , ((0, xK_Print), spawn "scrot")

           -- Restart xmonad and dzen2
         , ((mod4Mask, xK_q),
             spawn $ "if type xmonad; "
                  ++ "then xmonad --recompile && "
                  ++      "killall dzen2 && "
                  ++      "xmonad --restart; "
                  ++ "else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

           -- Vertical resizing with ResizableTall layout
         , ((mod4Mask, xK_u), sendMessage MirrorShrink)
         , ((mod4Mask, xK_i), sendMessage MirrorExpand)

           -- Misc silly stuff
         -- , ((mod4Mask .|. shiftMask, xK_h), helloWorld)

         ]
  where scratchPad = scratchpadSpawnActionTerminal myTerminal


-------------------------------------------------------------------------------
-- ScratchPad
-------------------------------------------------------------------------------
scratchpads = [ NS "newsbeuter" (myTerminal ++ " -e newsbeuter")
                                (title =? "newsbeuter")
                                centerFloating
              , NS "htop"       (myTerminal ++ " -e htop")
                                (title =? "htop")
                                centerFloating
              ]
    where centerFloating = customFloating $ W.RationalRect 0.2 0.2 0.6 0.6


-------------------------------------------------------------------------------
-- GridSelect
-------------------------------------------------------------------------------
myGSConfig = defaultGSConfig { gs_navigate   = myNavigation
                             , gs_cellheight = 60
                             , gs_cellwidth  = 250}
-- Custom navigation mode
myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = Map.fromList [
            ((0,        xK_Escape), cancel)
          , ((mod4Mask, xK_g),      cancel)
          , ((0,        xK_Return), select)
          , ((0,        xK_o),      select)
          , ((0,        xK_slash),  substringSearch myNavigation)
          , ((0,        xK_h),      move (-1,0)  >> myNavigation)
          , ((0,        xK_j),      move (0,1)   >> myNavigation)
          , ((0,        xK_k),      move (0,-1)  >> myNavigation)
          , ((0,        xK_l),      move (1,0)   >> myNavigation)
          , ((0,        xK_space),  setPos (0,0) >> myNavigation)
          ]
        -- The navigation handler ignores unknown key symbols
        navDefaultHandler = const myNavigation
