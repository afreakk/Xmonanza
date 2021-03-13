module AConfig (getConfig, AConfig(..), ifHnsTop) where
import Network.HostName

ifHnsTop AConfig{cl_hostName="hanstop"} thn _ = thn
ifHnsTop _ _ els = els

data AConfig = AConfig
  { cl_bg :: String
  , cl_bg0_h :: String
  , cl_bg0    :: String
  , cl_bg1      :: String
  , cl_grey  :: String
  , cl_red   :: String
  , cl_orange:: String
  , cl_yellow:: String
  , cl_green :: String
  , cl_aqua  :: String
  , cl_blue  :: String
  , cl_lilly :: String
  , cl_fg0   :: String
  , cl_fg1   :: String
  , cl_font :: String
  , cl_font_big :: String
  , cl_barHeight :: Int
  , cl_hostName :: String
  , cl_gsCellWidth :: Integer
  , cl_gsCellWidthBig :: Integer
  } deriving (Show)

getConfig :: IO AConfig
getConfig = do
  hostName <- getHostName
  return AConfig 
    { cl_bg = "#282828"
    , cl_bg0_h = "#1d2021"
    , cl_bg0   = "#282828"
    , cl_bg1   = "#3c3836"
    , cl_grey  = "#a89984"
    , cl_red   = "#cc241d"
    , cl_orange= "#d65d0e"
    , cl_yellow= "#d79921"
    , cl_green = "#98971a"
    , cl_aqua  = "#689d6a"
    , cl_blue  = "#468588"
    , cl_lilly = "#b16286"
    , cl_fg0   = "#fbf1c7"
    , cl_fg1   = "#fbdbb2"
    , cl_font  = "xft:Hack Nerd Font:size=15:Regular:antialias=true"
    , cl_font_big  = "xft:Hack Nerd Font:size=30:Regular:antialias=true"
    , cl_barHeight=if hostName == "hanstop" then 50 else 25
    , cl_hostName=hostName
    , cl_gsCellWidth = 400
    , cl_gsCellWidthBig = 500
    }
