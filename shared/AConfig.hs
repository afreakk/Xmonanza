module AConfig (getConfig, AConfig(..)) where

data AConfig = AConfig
  { cl_black :: String
  , cl_current_line :: String
  , cl_selection    :: String
  , cl_comment      :: String
  , cl_grey  :: String
  , cl_red   :: String
  , cl_orange:: String
  , cl_yellow:: String
  , cl_green :: String
  , cl_aqua  :: String
  , cl_blue  :: String
  , cl_lilly :: String
  , cl_font :: String
  , cl_barHeight :: Int
  } deriving (Show)

getConfig :: AConfig
getConfig = AConfig 
  { cl_black = "#2D2D2D"
  , cl_current_line = "#393939"
  , cl_selection    = "#515151"
  , cl_comment      = "#999999"
  , cl_grey  = "#CCCCCC"
  , cl_red   = "#F2777A"
  , cl_orange= "#f99157"
  , cl_yellow= "#ffcc66"
  , cl_green = "#99cc99"
  , cl_aqua  = "#66CCCC"
  , cl_blue  = "#6699cc"
  , cl_lilly = "#CC99CC"
  , cl_font  = "xft:Hack Nerd Font:size=13:Regular:antialias=true"
  , cl_barHeight=45
  }
