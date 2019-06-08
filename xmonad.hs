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

-- To get rid of all the gnome-flashback panels:
-- dconf write /org/gnome/gnome-panel/layout/toplevel-id-list "['']"
-- To get the top panel back, run:
-- dconf write /org/gnome/gnome-panel/layout/toplevel-id-list "['top-panel']"

-- To get gnome terminal profile from source system:
-- dconf dump /org/gnome/terminal/legacy/profiles:/ > gnome-terminal-profiles.dconf
--
-- To load it into the destination system:
-- dconf load /org/gnome/terminal/legacy/profiles:/ < gnome-terminal-profiles.dconf

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
myHooks = manageDocks 	<+>
	  myManageHook

-- LAYOUT MANAGEMENT --
-- Puts it all together
myLayout = smartBorders . avoidStruts $ reflectHoriz $ workspaceLayouts

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
	 , ((modm .|. shiftMask , xK_f) , sendMessage ToggleStruts)

	 -- Swaps the master window expand/shrink to correlate with reflected master
	 , ((modm , xK_h)		, sendMessage Expand)
	 , ((modm , xK_l)		, sendMessage Shrink)
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
  , ppTitle	    	= xmobarColor cPink ""
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
    , className =? "Caprine"		--> doShift (myWorkspaces !! 0)
    , className =? "Slack"		--> doShift (myWorkspaces !! 0)
-- note: this is Spotify, but it doesn't name itself until after it's created...
    , className =? ""			--> doShift (myWorkspaces !! 0)
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

-- General Color Palette
--
-- Warms (light to dark):
--      #D78787
--      #C56969
--      #D75F5F (saturated)
--      #9C5A5A
--      #944343
--      #7E3131 (dark mahogany)
--      #6A5555 (very gray pink)
--
-- Colds (light to dark):
--      #AFD7D7
--      #87AfAf
--      #91BAB1 (nice sea green)
--      #57716B (nice darker sea green)
--
-- Whites:
--      #D0D0D0
--      #AAAAAA
--
-- Blacks:
--      #1C1C1C

