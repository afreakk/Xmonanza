module AConfig (getConfig, AConfig(..)) where

data AConfig = AConfig
  { cl_black :: String
  , cl_grey :: String
  , cl_red :: String
  , cl_aqua :: String
  , cl_lilly :: String
  , cl_font :: String
  , cl_iconRoot :: String
  , cl_barHeight :: Int
  } deriving (Show)

getConfig :: AConfig
getConfig = AConfig 
  { cl_black = "#2D2D2D"
  , cl_grey  = "#CCCCCC"
  , cl_red   = "#F2777A"
  , cl_aqua  = "#66CCCC"
  , cl_lilly = "#CC99CC"
  , cl_font  = "xft:Hack:size=14:Regular:antialias=true"
  , cl_iconRoot="/home/afreak/icons"
  , cl_barHeight=25
  }

