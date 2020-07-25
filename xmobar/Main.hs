import Xmobar
import AConfig (getConfig, AConfig (..))

-- fcClr :: [Char] -> [Char] -> [Char]
-- fcClr clr str = "<fc="++clr++">"++str++"</fc>"

cmds :: AConfig -> [Runnable]
cmds cnf = 
  [ Run $ Network "wlp58s0"
    ["-L","0","-H","32000", "-m", "3",
     "--normal",cl_grey cnf,"--high",cl_red cnf,
     "-t", "<rx>KB /<tx>KB"] 50
  , Run $ MultiCpu
    ["--low", cl_aqua cnf, "--normal", cl_grey cnf,"--high",cl_red cnf,
     "-t", "<total>%"] 50
  , Run $ Memory
    [ "--normal",cl_grey cnf,"--high",cl_red cnf,
     "-m", "2", "-L", "0", "-H", "90",
     "-t","<usedratio>%"] 50
  , Run $ Date "%a %b %_d %H:%M" "date" 600
  , Run $ Alsa "default" "Master"
    [ "--low", cl_aqua cnf, "--normal",cl_grey cnf,"--high",cl_red cnf,
     "-t", "<volume>%"]
  , Run $ MultiCoreTemp
    ["--low", cl_aqua cnf, "--normal",cl_grey cnf,"--high",cl_red cnf,
     "-t", "<avg>°C"] 50
  , Run $ UnsafeStdinReader
  , Run $ BatteryP ["BAT0"]
    ["-t", "<acstatus><left>% <timeleft> <watts>",
     "-L", "10", "-H", "80", "-p", "3",
     "--",
     "-O", "\xf58e", "-o", "\xf58b", "-i", "",
     "-L", "-20", "-H", "-10",
     "-l", cl_grey cnf, "-m", cl_aqua cnf, "-h", cl_red cnf, "-p", cl_green cnf,
     "-a", "notify-send -u critical 'Battery running out!!'",
     "-A", "3"]
    100
  ]

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
         , commands = cmds cnf
         , sepChar = "%"
         , alignSep = "}{"
         , template = "%UnsafeStdinReader%}\
                      \{奔 %alsa:default:Master% | ﯱ %wlp58s0% | %battery% | \xf85a %memory% | \xfb19 %multicpu% %multicoretemp% | %date%"
         }

main :: IO ()
main = xmobar $ config getConfig

