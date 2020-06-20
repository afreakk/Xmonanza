import Xmobar
import AConfig (getConfig, AConfig (..))

config :: AConfig -> Config
config cnf =
  Config { verbose = False
         , wmClass = "xmobar"
         , wmName = "xmobar"
         , border = NoBorder
         , borderColor = "#BFBFBF"
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
         , iconRoot = cl_iconRoot cnf
         , allDesktops = True
         , overrideRedirect = True
         , commands = [ Run $ Network "enp0s31f6" ["-L","0","-H","32000", "-m", "3", "--normal",cl_aqua cnf,"--high",cl_red cnf, "-t", "<rx>KB|<tx>KB"] 50
                      , Run $ MultiCpu [ "--low", cl_aqua cnf, "--normal",cl_aqua cnf,"--high",cl_red cnf, "-t", "<total>%"] 50
                      , Run $ Memory ["-t","<usedratio>%", "-L", "0", "-H", "90", "--normal",cl_aqua cnf,"--high",cl_red cnf, "-m", "2"] 50
                      , Run $ Date "%a %b %_d %H:%M" "date" 600
                      , Run $ Alsa "default" "Master" ["-t", "<volume>%",  "--low", cl_aqua cnf, "--normal",cl_aqua cnf,"--high",cl_red cnf]
                      , Run $ Com "nvidia-settings" ["-t","-q","[gpu:0]/GPUCoreTemp" ] "gputemp" 50
                      , Run $ MultiCoreTemp [ "--low", cl_aqua cnf, "--normal",cl_aqua cnf,"--high",cl_red cnf, "-t", "<avg>°C"] 50
                      , Run $ CatInt 0 "/sys/devices/platform/nct6775.2592/hwmon/hwmon3/fan2_input" [
                        "-L", "0", "-H", "1082", "--normal",cl_aqua cnf,"--high",cl_red cnf] 50
                      , Run $ StdinReader
                      ]
         , sepChar = "%"
         , alignSep = "}{"
         , template = "%StdinReader%}\
                      \{<icon=volume.xpm/> %alsa:default:Master% | %enp0s31f6% | <icon=memory.xpm/> %memory% | <icon=cpu.xpm/> %multicpu% %multicoretemp% %cat0% | <icon=gpu.xpm/> %gputemp%°C | <fc=" ++ cl_lilly cnf ++ ">%date%</fc>"
         }

main :: IO ()
main = xmobar $ config getConfig

