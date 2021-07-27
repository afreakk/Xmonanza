module AConfig (getConfig, AConfig(..), ifHnsTop) where
import Network.HostName

ifHnsTop AConfig{cl_hostName="hanstop"} thn _ = thn
ifHnsTop _ _ els = els

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
  } deriving (Show)

getConfig :: IO AConfig
getConfig = _getConfig <$> getHostName
  where _getConfig hostName = AConfig
          { cl_bg = ifIsLightTheme "#fbf1c7" "#282828"
          , cl_red   = "#cc241d"
          , cl_orange= "#d65d0e"
          , cl_green = "#98971a"
          , cl_aqua  = "#689d6a"
          , cl_lilly = "#b16286"
          , cl_fg0   = ifIsLightTheme "#282828" "#fbf1c7"
          , cl_font  = "xft:Hack Nerd Font:size=15:Regular:antialias=true"
          , cl_font_big  = "xft:Hack Nerd Font:size=30:Regular:antialias=true"
          , cl_barHeight= ifIsHanstop 55 25
          , cl_hostName=hostName
          , cl_gsCellWidth = ifIsHanstop 500 400
          , cl_gsCellWidthBig = ifIsHanstop 750 500
          , cl_gsCellHeightBig = ifIsHanstop 120 80
          }
          where
          ifIsLightTheme lightValue darkValue = case hostName of
            -- "hanstop" -> lightValue
            _         -> darkValue
          ifIsHanstop isHanstopValue elseValue =
            if hostName == "hanstop" then isHanstopValue else elseValue
