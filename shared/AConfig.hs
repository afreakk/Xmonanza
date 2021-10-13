module AConfig (getConfig, AConfig(..), HstNm(..), hstNmCond) where
import Network.HostName hiding (HostName)
import SimpleCmd
import Data.List
import Text.Read
import Data.Maybe

data HostName = Hanstop | Nimbus2k | Hogwarts | Other deriving (Show)

hstNmCond :: AConfig -> HstNm a -> a
hstNmCond AConfig{cl_hostName=Hanstop} x = hst_hanstop x
hstNmCond AConfig{cl_hostName=Hogwarts} x = hst_hogwarts x
hstNmCond AConfig{cl_hostName=Nimbus2k} x = hst_nimbus2k x
hstNmCond _ x = hst_other x

data HstNm a = HstNm
  { hst_hogwarts :: a
  , hst_hanstop :: a
  , hst_nimbus2k :: a
  , hst_other :: a
  }

stringToHostName "hanstop" = Hanstop
stringToHostName "hogwarts" = Hogwarts
stringToHostName "nimbus2k" = Nimbus2k
stringToHostName _ = Other

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
  , cl_hostName :: HostName
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
  hostName <- fmap stringToHostName getHostName
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
          , cl_font  = "xft:Hack Nerd Font:size=12:Regular:antialias=true"
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
