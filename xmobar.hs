Config { 

   -- https://github.com/jaor/xmobar
   
   ---- COLOR PALETTE --
   -- Red1 : dark gray red
   -- Red2 : orange red
   -- Gr1  : gray green
   -- Gr2  : lighter gray green
   -- cWhite 	= "#DFDFDF"
   -- cGray     = "#969595"
   -- cBlack    = "#1C1C1C"
   -- cRed1	    = "#AF8787"
   -- cRed2	    = "#D75F5F"
   -- cGreen1	= "#87AFAF"
   -- cGreen2	= "#AFD7D7"

   -- appearance
     font 	= "xft:inconsolata:size=10:antialias=true"
   , bgColor 	= "black"   -- background of bar color
   , fgColor 	= "#DFDFDF"    -- standard element font color
   , position 	= TopP 0 0
   , border 	= NoBorder
   
   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = " %StdinReader%}{ %KSJC% | %memory% | %date% "

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      False   -- show on all desktops
   , overrideRedirect = False   -- set the Override Redirect flag (Xlib)
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
    [ Run Memory         [ "--template" ,"mem: <usedratio>"
                          , "--Low"      , "20"        -- units: %
                          , "--High"     , "90"        -- units: %
                          , "--low"      , "#87AFAF"
                          , "--normal"   , "#AFD7D7"
                          , "--high"     , "#AF8787"
                         ] 30

    -- time and date indicator 
    --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
    , Run Date  "<fc=#DFDFDF>%a, %b %d %Y | %r</fc>" "date" 10

    -- weather monitor (18000 refresh every 30 min) [KRHV][KSJC] 
    , Run Weather "KSJC"  -- Don't forget to change above in template too
                 ["-t", "<tempF>°F | <skyCondition>"
                 , "-L"         , "65"
                 , "-H"         , "85"
                 , "--high"     , "#AF8787"
                 , "--normal"   , "#AFD7D7"
                 , "--low"      , "#87AFAF"
                 ] 18000

	-- status about which layout and workspace
	, Run StdinReader
        ]
   } 


