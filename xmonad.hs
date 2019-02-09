import XMonad
import XMonad.Config.Gnome
-- keybindings
import qualified Data.Map as M
-- layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import ReflectSilent
import XMonad.Layout.PerWorkspace
-- hooks
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageHelpers
-- misc
import qualified XMonad.StackSet as W
import Control.Monad
import Data.Monoid (All (All))

-- dconf write /org/gnome/gnome-panel/layout/toplevel-id-list "['']"
-- To get the top panel back, run:
-- dconf write /org/gnome/gnome-panel/layout/toplevel-id-list "['top-panel']"

-- COLOR PALETTE --
cTeal 	= "#3FB8AF"
cGreen	= "#7FC7AF"
cLinen	= "#DAD8A7"
cPink	= "#FF9E9D"
cCoral	= "#FF3D7F"

-- WORKSPACES --
myWorkspaces = ["1","2"] ++ map show [3..9]

-- LAUNCHER --
-- Note: the cache for yeganesh is stored in ~/.local/share/yeganesh
-- 	 so if an entry needs to be forgotten, that's where to go to delete it
myLauncher = "$(yeganesh -x -- -nb black -fn inconsolata:size=10 -nf \\#DAD8A7 -sb black -sf \\#3FB8AF)"

-- MANAGE HOOKS --
myHooks = manageDocks <+>
	  myManageHook

-- LAYOUT MANAGEMENT --
-- Puts it all together
myLayout = avoidStruts $ reflectHoriz $ workspaceLayouts

-- Per-workspace layouts, with tweak-ability for easy management
workspaceLayouts =
	onWorkspace "1" msgLayouts $
	defaultLayouts
	where
		msgLayouts 	 = tabbedLayout
		defaultLayouts 	 = tiledLayout ||| tabbedLayout ||| fullscreenLayout
	
		tiledLayout 	 = renamed [Replace "[ | ]"] (Tall 1 (2/100) (1/2))
		tabbedLayout 	 = renamed [Replace "[===]"] (tabbed shrinkText tabConfig)
		fullscreenLayout = renamed [Replace "[   ]"] (Full)


-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = cPink,
    activeTextColor = cPink,
    activeColor = "#555152",
    inactiveBorderColor = "#2E2633",
    inactiveTextColor = "#555152",
    inactiveColor = "#2E2633"
}

-- CUSTOM KEYBINDINGS --
myKeys conf@(XConfig {XMonad.modMask = modm}) =
             [
	 -- Some standard keybindings
	   ((modm , xK_Escape)	        , kill)  -- xK_grave -- another option
	 , ((modm , xK_backslash)       , spawn "gnome-terminal")
         , ((modm , xK_x)               , spawn "firefox")
         , ((modm , xK_f)               , spawn "nautilus")
         , ((modm , xK_Delete)          , spawn "gnome-system-monitor")
	 , ((modm , xK_p)		, spawn myLauncher)
	 , ((modm .|. shiftMask , xK_q)	, spawn "gnome-session-quit --power-off")
	 , ((modm .|. shiftMask , xK_r) , spawn "gnome-session-quit --reboot")

	 -- Swaps the master window expand/shrink to correlate with reflected master
	 , ((modm , xK_h)		, sendMessage Expand)
	 , ((modm , xK_l)		, sendMessage Shrink)

	 -- Power management shit
	 , ((modm .|. shiftMask, xK_l)  , spawn "~/.local/bin/i3lock-plant")
             ]
-- Takes the union of default keys and custom keys, with custom keys
-- having the ability to override defaults
newKeys x = M.union (M.fromList (myKeys x)) (keys defaultConfig x)

-- CUSTOM MOUSEBINDINGS --
myMouse (XConfig {XMonad.modMask = modm}) = M.fromList $
	     [
	 -- move window
	   ((modm, button1)		, (\w -> focus w >> mouseMoveWindow w))

	 -- resize window
	 , ((modm .|. shiftMask, button1) , (\w -> focus w >> mouseResizeWindow w))
	     ]

-- XMOBAR STUFF --
myXMob = "xmobar ~/.xmonad/xmobar.hs"
myLogHook h = (dynamicLogWithPP $ myPP h)
  
myPP h = xmobarPP
  { ppCurrent		= xmobarColor cGreen "" . wrap "[" "]"
  , ppVisible		= xmobarColor cPink ""
  , ppTitle		= xmobarColor cPink ""
  , ppOutput		= hPutStrLn h
  }
  
-- MANAGE HOOKS --
-- 
-- To find the property name associated with a program, use
-- $ xprop | grep WM_CLASS
-- and click on the client you're interested in.
-- 
myManageHook = composeAll
    [ manageHook gnomeConfig
-- Unity 2d related
    , className =? "Unity-2d-panel" 	--> doIgnore
    , className =? "Unity-2d-launcher" 	--> doIgnore
-- more hooks:
    , className =? "Thunderbird"	--> doShift (myWorkspaces !! 0)
    , className =? "yakyak"		--> doShift (myWorkspaces !! 0)
    , className =? "Caprine"		--> doShift (myWorkspaces !! 0)
    , className =? "Slack"		--> doShift (myWorkspaces !! 0)
    ]

-- THE MAIN THING THAT DOES THE THING --
main = do
  h <- spawnPipe myXMob
  xmonad $ gnomeConfig
    { terminal    		= "gnome-terminal"
    , modMask     		= mod1Mask
    , focusFollowsMouse 	= False 
    , borderWidth 		= 1
    , normalBorderColor 	= "#000000"
    , focusedBorderColor 	= "#825f69" -- "#e73a6f" -- salmonish pink --  
    , workspaces		= myWorkspaces
    , layoutHook 		= myLayout
    , keys       		= newKeys
    , mouseBindings		= myMouse
    , manageHook 		= myHooks 
    , logHook	 		= myLogHook h
    , startupHook		= startupHook gnomeConfig >> setWMName "LG3D"
    }


