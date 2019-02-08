Config { 

   -- https://github.com/jaor/xmobar

   -- appearance
     font 	= "xft:inconsolata:size=10:antialias=true"
   , bgColor 	= "black"
   , fgColor 	= "#DAD8A7"
   , position 	= Top
   , border 	= NoBorder
   
   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = " %StdinReader%}{ %wlp58s0wi% | %battery% | %memory% || %date% "

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = False    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
   , commands = 

        -- memory usage monitor
        [ Run Memory         [ "--template" ,"M: <usedratio>%"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , "#3FB8AF"
                             , "--normal"   , "#7FC7AF"
                             , "--high"     , "#FF3D7F"
                             ] 30

        -- battery monitor
        , Run Battery        [ "--template" , "P: <acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#FF3D7F"
                             , "--normal"   , "#7FC7AF"
                             , "--high"     , "#3FB8AF"
                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "<left>%" 
                                       -- AC "on" status
                                       , "-O"	, "<fc=#FF9E9D>Charging</fc> (<left>%)"
                                       -- charged status
                                       , "-i"	, "<fc=#3FB8AF>Charged</fc>"
                             ] 100

	-- wireless network monitor
	-- wlp58s0 is my wireless lan card name
	-- wireless lan pci part 58 slot 0
	, Run Wireless "wlp58s0" [ "--template" , "<essid> [<qualitybar>]"
			     , "--Low"	    , "20"
			     , "--High"     , "80"
			     , "--low"	    , "#FF3D7F"
			     , "--normal"   , "#7FC7AF"
			     , "--high"	    , "#3FB8AF"
			     ] 100

        -- time and date indicator 
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run Date           "<fc=#DAD8A7>%F (%a) %r</fc>" "date" 10

	-- status about which layout and workspace
	, Run StdinReader
        ]
   } 


