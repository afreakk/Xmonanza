module XmobarUtils (xmobarShorten) where


xmobarShorten :: Int -> String -> String
xmobarShorten maxLen str
  | visibleStrLen > maxLen = shortenedStr ++ ".."
  | otherwise              = shortenedStr
  where
    (shortenedStr, visibleStrLen) = shorten False str "" 0

    shorten :: Bool -> String -> String -> Int -> (String, Int)
    shorten isTag ('<':xs) str idx = shorten True  xs (str++"<")  idx
    shorten isTag ('>':xs) str idx = shorten False xs (str++">")  idx
    shorten False (x:xs)   str idx
      | idx < maxLen               = shorten False xs (str++[x]) (idx+1)
      | otherwise                  = shorten False xs  str       (idx+1)
    shorten True (x:xs)    str idx = shorten True  xs (str++[x])  idx
    shorten isTag []       str idx = (str, idx)
