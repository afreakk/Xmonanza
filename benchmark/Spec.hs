import Criterion.Main
import XmobarUtils (xmobarShorten)


-- Our benchmark harness.
main :: IO ()
main = defaultMain [
    bgroup "xmobarShorten"
    [ bench "1shorten"  $ whnf ( xmobarShorten 1 ) "<action=whatever>ya</action>"
    , bench "2shorten"  $ whnf ( xmobarShorten 10 ) "<action=whatever><action>hmmm</action>yup</action>"
    , bench "3shorten"  $ whnf (xmobarShorten 20) "<action=whatever>idd<action>wft</action></action><fc>lolw</fc><fc>yal</fc><fc>yal</fc><fc>yal</fc><fc>yal</fc>"
    ]
  ]
