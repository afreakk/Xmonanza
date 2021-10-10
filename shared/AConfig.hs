module AConfig (getConfig, AConfig(..), ifLaptop, HstNm(..), hstNmCond) where
import Network.HostName
import SimpleCmd
import Data.List
import Text.Read
import Data.Maybe

ifLaptop AConfig{cl_hostName="hanstop"} thn _ = thn
ifLaptop AConfig{cl_hostName="nimbus2k"} thn _ = thn
ifLaptop _ _ els = els

hstNmCond :: AConfig -> HstNm a -> a
hstNmCond AConfig{cl_hostName="hanstop"} x = hst_hanstop x
hstNmCond AConfig{cl_hostName="hogwarts"} x = hst_hogwarts x
hstNmCond AConfig{cl_hostName="nimbus2k"} x = hst_nimbus2k x
hstNmCond _ x = hst_hogwarts x

data HstNm a = HstNm
  { hst_hogwarts :: a
  , hst_hanstop :: a
  , hst_nimbus2k :: a
  }

data AConfig = AConfig
  { cl_bg :: String
  , cl_red   :: String
  , cl_orange:: String
  , cl_green :: String
  , cl_aqua  :: String
  , cl_lilly :: String
  , cl_fg0   :: String
  , cl_font :: String
  , cl_font_big :: String
  , cl_barHeight :: Int
  , cl_hostName :: String
  , cl_gsCellWidth :: Integer
  , cl_gsCellWidthBig :: Integer
  , cl_gsCellHeightBig :: Integer
  , cl_gsCellHeight :: Integer
  , cl_dpi :: Integer
  } deriving (Show)

headWithDefault [] = "Xft.dpi:        200"
headWithDefault x = head x

sndOfThing [fst, snd] = snd
sndOfThing x = "200"

getConfig :: IO AConfig
getConfig = do
  hostName <- getHostName 
  --xrdbQStr <- cmd "xrdb" ["-query"] 
  --let xrdbQStr = fromStdout xrdbOut
 -- let lined = lines  xrdbQStr
  --let dpiLine = headWithDefault $ filter (isInfixOf "Xft.dpi:") lined
  --let dpiStr = sndOfThing (words dpiLine)
  --let dpi = readMaybe dpiStr :: Maybe Integer
  let dpi = Just 100
  return $ _getConfig hostName dpi
  where _getConfig hostName dpi = AConfig
          { cl_bg = ifIsLightTheme "#fbf1c7" "#282828"
          , cl_red   = "#cc241d"
          , cl_orange= "#d65d0e"
          , cl_green = "#98971a"
          , cl_aqua  = "#689d6a"
          , cl_lilly = "#b16286"
          , cl_fg0   = ifIsLightTheme "#282828" "#fbf1c7"
          , cl_font  = "xft:Hack Nerd Font:size=15:Regular:antialias=true"
          , cl_font_big  = "xft:Hack Nerd Font:size=30:Regular:antialias=true"
          , cl_barHeight= 25
          , cl_hostName=hostName
          , cl_gsCellWidth = 400
          , cl_gsCellWidthBig = 500
          , cl_gsCellHeight = 50
          , cl_gsCellHeightBig = 80
          , cl_dpi = fromMaybe 200 dpi
          }
          where
          ifIsLightTheme lightValue darkValue = case hostName of
            -- "hanstop" -> lightValue
            _         -> darkValue
          ifIsLaptop laptopValue elseValue =
            if hostName == "hanstop" || hostName == "nimbus2k" then laptopValue else elseValue
