import XMonad
import XMonad.Config.Gnome
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicBars
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Actions.OnScreen
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Control.Monad
import Data.Monoid (All (All))
import ReflectSilent

-- To get rid of all the gnome-flashback panels:
-- dconf write /org/gnome/gnome-panel/layout/toplevel-id-list "['']"
-- To get the top panel back, run:
-- dconf write /org/gnome/gnome-panel/layout/toplevel-id-list "['top-panel']"

-- To get gnome terminal profile from source system:
-- dconf dump /org/gnome/terminal/legacy/profiles:/ > gnome-terminal-profiles.dconf
--
-- To load it into the destination system:
-- dconf load /org/gnome/terminal/legacy/profiles:/ < gnome-terminal-profiles.dconf

-- COLOR PALETTE
cWhite  = "#DFDFDF"
cGray   = "#969595"
cBlack  = "#1C1C1C"
cRed1   = "#AF8787"
cRed2   = "#D78787"
cGreen1 = "#87AFAF"
cGreen2 = "#AFD7D7"

------------------------------------------------------------------------------------------
-- MAIN                                                                                  -
------------------------------------------------------------------------------------------

main = do
    h <- spawnPipe $ myXMob
    xmonad $ gnomeConfig
        { terminal    		    = "gnome-terminal"
        , modMask     		    = mod1Mask
        , focusFollowsMouse 	= False 
        , borderWidth 		    = 1
        , normalBorderColor 	= "#000000"
        , focusedBorderColor 	= cRed1
        , workspaces		    = myWorkspaces
        , layoutHook 		    = myLayout
        , keys       		    = newKeys
        , mouseBindings		    = myMouse
        , manageHook 		    = myHooks 
        , logHook	 		    = myLogHook h
        , startupHook		    = startupHook gnomeConfig >> setWMName "LG3D"
        }

-- LAUNCHER
-- Note: the cache for yeganesh is stored in ~/.local/share/yeganesh
-- so if an entry needs to be forgotten, that's where to go to delete it
myLauncher = "$(yeganesh -x -- -nb black -fn inconsolata:size=10 -nf \\#DFDFDF -sb black -sf \\#87AFAF)"

-- XMOBAR
myXMob = "xmobar ~/.xmonad/xmobar.hs"
myLogHook h = (dynamicLogWithPP $ myPP h)
  
myPP h = xmobarPP
    { ppCurrent     = xmobarColor cGreen1 "" . wrap "[" "]"
    , ppVisible     = xmobarColor cRed2 ""
    , ppTitle       = xmobarColor cRed2 ""
    , ppOutput      = hPutStrLn h
    }

------------------------------------------------------------------------------------------
-- WORKSPACES                                                                            -
------------------------------------------------------------------------------------------

myWorkspaces = ["1","2"] ++ map show [3..9]

-- LAYOUT MANAGEMENT
myLayout = smartBorders . avoidStruts $ reflectHoriz $ workspaceLayouts

-- Per-workspace layouts
workspaceLayouts =
    onWorkspace "1" msgLayouts $
    defaultLayouts
    where
        msgLayouts       = rTiledLayout ||| tabbedLayout
        defaultLayouts   = rTiledLayout ||| tabbedLayout ||| fullscreenLayout

        --tiledLayout      = renamed [Replace "[ | ]"] (Tall 1 (2/100) (1/2))
        tabbedLayout     = renamed [Replace "[===]"] (tabbed shrinkText tabConfig)
        fullscreenLayout = renamed [Replace "[   ]"] (Full)
        rTiledLayout     = renamed [Replace "[ + ]"] (ResizableTall 1 (2/100) (1/2) [])

-- Tabbed layout configuration, color settings
tabConfig = defaultTheme {
    activeBorderColor   = cGray,
    activeTextColor     = cWhite,
    activeColor         = cBlack,
    inactiveBorderColor = cBlack,
    inactiveTextColor   = cGray,
    inactiveColor       = cBlack
}

------------------------------------------------------------------------------------------
-- KEY BINDINGS                                                                          -
------------------------------------------------------------------------------------------

-- Union of default and custom, custom overriding defaults
newKeys x = M.union (M.fromList (myKeys x)) (keys defaultConfig x)

myKeys conf@(XConfig {XMonad.modMask = modm}) =
    -- Spawning applications
    [ ((modm , xK_Escape)           , kill)
    , ((modm , xK_backslash)        , spawn "gnome-terminal")
    , ((modm , xK_x)                , spawn "firefox")
    , ((modm , xK_f)                , spawn "nautilus --new-window")
    , ((modm , xK_Delete)           , spawn "gnome-system-monitor")
    , ((modm , xK_p)                , spawn myLauncher)
    , ((modm .|. shiftMask , xK_q)  , spawn "gnome-session-quit --power-off")
    , ((modm .|. shiftMask , xK_r)  , spawn "gnome-session-quit --reboot")
    , ((modm .|. shiftMask , xK_f)  , sendMessage ToggleStruts)

    -- Swaps direction to correlate with master on right
    , ((modm , xK_h)        , sendMessage Expand)
    , ((modm , xK_l)        , sendMessage Shrink)
    , ((modm , xK_comma)    , sendMessage (IncMasterN (-1)))
    , ((modm , xK_period)   , sendMessage (IncMasterN 1))
    -- Set screens to default (workspace 1 on left, workspace 2 on right)
    , ((modm , xK_w)        , windows (greedyViewOnScreen 1 "1" . greedyViewOnScreen 0 "2"))
    -- Resizable tile key bindings for mirror
    , ((modm , xK_z)        , sendMessage MirrorShrink)
    , ((modm , xK_a)        , sendMessage MirrorExpand)
    ]
    ++
    -- Workspace cycling for dual monitors
    [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

-- Mouse bindings
myMouse (XConfig {XMonad.modMask = modm}) = M.fromList $
    [
    -- Move window
    ((modm, button1) , (\w -> focus w >> mouseMoveWindow w))
    -- Resize window
    , ((modm .|. shiftMask, button1) , (\w -> focus w >> mouseResizeWindow w))
    ]

------------------------------------------------------------------------------------------
-- COMPOSITION                                                                           -
------------------------------------------------------------------------------------------
-- To find the property name associated with a program, use
-- $ xprop | grep WM_CLASS
-- and click on the client you're interested in.

myHooks = manageDocks <+> myManageHook
myManageHook = composeAll
    [ manageHook gnomeConfig
    -- Unity 2d related
    , className =? "Unity-2d-panel"     --> doIgnore
    , className =? "Unity-2d-launcher"  --> doIgnore
    -- Manage programs:
    , className =? "Caprine"            --> doShift (myWorkspaces !! 0)
    , className =? "Slack"              --> doShift (myWorkspaces !! 0)
    -- Spotify
    , className =? ""                   --> doShift (myWorkspaces !! 0)
    -- Floating terminal
    , className =? "Gnome-terminal"     --> doRectFloat (W.RationalRect l t w h)
    ]
    where
        h = 0.30        -- terminal height
        w = 0.45        -- terminal width
        t = (1-h)*1/2   -- distance from top edge
        l = (1-w)*1/2   -- distance from left edge

