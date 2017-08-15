{-# LANGUAGE CPP #-}
 
{-
#include <X11/XF86keysym.h>
-}

import Data.Map hiding (keys, map)
import XMonad
import XMonad.Layout.Fullscreen
import qualified XMonad.Core as Core
import XMonad.Config.Gnome
import XMonad.Actions.PerWorkspaceKeys
import XMonad.StackSet
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Actions.CycleWS
import XMonad.Util.Run(spawnPipe)

main :: IO ()
main = do
    xmproc2 <- spawnPipe "/home/timur/.xmonad/x.sh"
    xmproc3 <- spawnPipe "feh --bg-fill ~/devushki-erotika-igry-22044.jpg"
    xmonad . fullscreenSupport =<< xmobar gnomeConfig
        { terminal          = "gnome-terminal"
        , modMask           = mod4Mask
        , Core.workspaces   = myWorkspaces
        , borderWidth       = 0
        , clickJustFocuses  = True
        , keys              = myKeys
        , manageHook        = composeAll myManageHook <+> manageHook defaultConfig
        , layoutHook        = myLayoutHook
        }

myLayoutHook = three tiled mirror Full

three first second third = desktopLayoutModifiers $ first ||| second ||| third
 
tiled :: Tall a
tiled = Tall 1 (3 / 100) $ 1 / 2
 
mirror :: Mirror Tall a
mirror = Mirror tiled

myWorkspaces :: [String]
myWorkspaces =
    [ "web", "terminal", "code" , "windows"
    , "office", "music", "video"
    , "8", "9"
    ]

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf =
    let
        mask :: KeyMask
        mask = modMask conf
        
        mS :: KeyMask
        mS = mask .|. shiftMask
    in
        union 
        (
            fromList $
                [ ((mask, xK_Left)  , prevWS)
                , ((mask, xK_Right) , nextWS)
                , ((mask, xK_a)     , prevWS)
                , ((mask, xK_s)     , nextWS)
                , ((mask, xK_Tab)   , toggleWS)
                , ((mS  , xK_Left)  , shiftToPrev)
                , ((mS  , xK_Right) , shiftToNext)
                , ((mask, xK_p)     , spawn "dmenu_run")
                , ((mask, xK_c)     , spawn "gnome-terminal")
                , ((mask, xK_e)     , spawn "nautilus")
                , ((mS  , xK_b)     , spawn "google-chrome-stable")
                , ((mask, xK_m)     , spawn "rhythmbox")
                -- , ((mS  , xK_q)     , spawn "kill -9 -1")
                , shortcut mS "shutdown" xK_o
                , ((noModMask, XF86XK_Mail), spawn "google-chrome-stable e.mail.ru")
                ]
            ++ multimedia
        ) $
        keys defaultConfig conf
 
shortcut :: MonadIO x => keyMask -> String -> keySym -> ((keyMask, keySym), x ())
shortcut keyMask command keySym = ((keyMask, keySym), spawn command)
 
multimedia :: [((KeyMask, KeySym), X ())]
multimedia =
    [ player "previous" XF86XK_AudioPrev
    , player "next" XF86XK_AudioNext
    , player "play-pause" XF86XK_AudioPlay
    , player "stop" XF86XK_AudioStop
    , amixer "toggle" XF86XK_AudioMute
    , volume "-" XF86XK_AudioLowerVolume
    , volume "+" XF86XK_AudioRaiseVolume
    , noMask "rhythmbox" XF86XK_Music
    ]
 
player :: String -> keySym -> ((KeyMask, keySym), X ())
player command = noMask $ "rhythmbox-client --no-start --" ++ command
 
noMask :: String -> keySym -> ((KeyMask, keySym), X ())
noMask = shortcut noModMask
 
amixer :: String -> keySym -> ((KeyMask, keySym), X ())
amixer option = noMask $ "amixer set Master playback" ++ option
 
volume :: String -> keySym -> ((KeyMask, keySym), X ())
volume signal = noMask $ "amixer sset 'Master' 2%" ++ signal

myManageHook :: [ManageHook]
myManageHook =
    [ associate "terminal" "urxvt"
    , music "Rhythmbox"
    , video "Vlc"
    , web "Google Chrome"
    , office "LibreOffice"
    , isFullscreen --> doFullFloat
    ]

associate :: WorkspaceId -> String -> ManageHook
associate area wmClass = className =? wmClass --> doShift area
 
code :: String -> ManageHook
code = associate "code"
 
music :: String -> ManageHook
music = associate "music"

video :: String -> ManageHook
video = associate "video"

office :: String -> ManageHook
office = associate "office"

web :: String -> ManageHook
web = associate "web"