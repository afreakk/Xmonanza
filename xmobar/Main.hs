import Xmobar
import AConfig (ifHnsTop, getConfig, AConfig (..))

-- fcClr :: [Char] -> [Char] -> [Char]
-- fcClr clr str = "<fc="++clr++">"++str++"</fc>"

cmds :: AConfig -> [Runnable]
cmds cnf = 
  [ Run $ DynNetwork
    ["-L", "0",
     "-H", "32000",
     "--normal", cl_grey cnf,
     "--high",cl_red cnf,
     "-t", "<rxvbar> <txvbar>"
    ] 50
  , Run $ MultiCpu
    ["-L", "0",
     "--minwidth", "2",
     "--low", cl_aqua cnf,
     "--normal", cl_grey cnf,
     "--high", cl_red cnf,
     "-t", "<total>%"
    ] 50
  , Run $ Memory
    ["--normal", cl_grey cnf,
     "--high", cl_red cnf,
     "--minwidth", "2",
     "-m", "2",
     "-L", "0",
     "-H", "90",
     "-t","<usedratio>%"
    ] 50
  , Run $ Date "%a %d %b %H:%M" "date" 600
  , Run $ Alsa "default" "Master"
    ["--low", cl_grey cnf,
     "--normal", cl_grey cnf,
     "--high", cl_red cnf,
     "-H", "100",
     "-t", "<status> <volume>%",
     "--minwidth", "2",
     "--",
     "--highs", "墳",
     "--mediums", "奔",
     "--lows", "奄",
     "--off", "婢",
     "--on", "",
     "--onc", cl_grey cnf,
     "--offc", cl_red cnf
    ]
  , Run $ MultiCoreTemp
    ["-L", "25",
     "-H", "75",
     "--minwidth", "2",
     "--low", cl_aqua cnf,
     "--normal", cl_grey cnf,
     "--high", cl_red cnf,
     "-t", "<avg>°C"
    ] 50
  , Run $ WeatherX "ENZV"
    [ ("clear", "望")
    , ("sunny", "\xe30d")
    , ("mostly clear", "\xe37b")
    , ("mostly sunny", "\xe30c")
    , ("partly sunny", "\xe302")
    , ("fair", "\xe302")
    , ("cloudy","\xe33d")
    , ("overcast","\xe33d")
    , ("partly cloudy", "\xe379")
    , ("mostly cloudy", "\xe37e")
    , ("considerable cloudiness", "\xfa8f")]
  ["-t", "<skyConditionS> <tempC>° <windKmh>km/h"] 600
  , Run $ UnsafeStdinReader
  ]

laptopCmds :: AConfig -> [Runnable]
laptopCmds cnf = [
  Run $ BatteryP ["BAT0"]
    ["-t", "<leftipat>",
     "-L", "10", "-H", "80", "-p", "3",
     "--minwidth", "3",
     "--",
     "--on-icon-pattern", "\xf58e<left>% <timeleft> <watts>",
     "--off-icon-pattern", "\xf58b<left>% <timeleft> <watts>",
     "--idle-icon-pattern", "\xf578",
     "-L", "-20", "-H", "-10",
     "-l", cl_grey cnf, "-m", cl_aqua cnf, "-h", cl_red cnf, "-p", cl_green cnf,
     "-a", "notify-send -u critical 'Battery running out!!'",
     "-A", "3"]
    50
  ]

stationaryCmds cnf = [
  Run $ Com "nvidia-settings"
    ["-t","-q","[gpu:0]/GPUCoreTemp" ]
    "gputemp" 50
  ]

laptopTmpl =
  "%UnsafeStdinReader%}\
  \{ %ENZV% | %alsa:default:Master% | ﯱ %dynnetwork% | %battery% | \xf85a %memory% | \xfb19 %multicpu% %multicoretemp% | %date%"

stationaryTmpl = 
  "%UnsafeStdinReader%}\
  \{ %ENZV% | <action=`setSinkVolumeDefault.sh +1db` button=4><action=`setSinkVolumeDefault.sh -1db` button=5>%alsa:default:Master%</action></action> | ﯱ %dynnetwork% | \xf7e8 %gputemp%°C | \xf85a %memory% | \xfb19 %multicpu% %multicoretemp% | <action=`~/bin/runner.sh` button=1>%date%</action>"

config :: AConfig -> Config
config cnf =
  Config { verbose = False
         , wmClass = "xmobar"
         , wmName = "xmobar"
         , border = NoBorder
         , borderColor = cl_orange cnf
         , borderWidth = 1
         , textOffsets = []
         , font = cl_font cnf
         , additionalFonts = []
         , bgColor = cl_black cnf
         , fgColor = cl_grey cnf
         , alpha = 150
         -- , position = Top
         , position = TopSize L 100 (cl_barHeight cnf)
         , textOffset = -1
         , iconOffset = -1
         , lowerOnStart = True
         , pickBroadest = False
         , persistent = False
         , hideOnStart = False
         , iconRoot = ""
         , allDesktops = True
         , overrideRedirect = True
         , commands = cmds cnf ++ ifHnsTop cnf
            (laptopCmds cnf)
            (stationaryCmds cnf)
         , sepChar = "%"
         , alignSep = "}{"
         , template = ifHnsTop cnf laptopTmpl stationaryTmpl
         }

main :: IO ()
main = do
  cnf <- getConfig
  xmobar $ config cnf

