# Xmonanza
##### XMonad and Xmobar monorepo for sharing code between them, build with stack

1. Edit `src/AConfig.hs` (atleast iconpath (check xmobar/Main.hs for icons used))
1. `stack install`
    `xmonad-afreak` and `xmobar-afreak` will then be created in `~/.local/bin` or `local-bin-path`
    (`~/.local/bin` needs to be in `$PATH`)
1. Copy custom buildscript for xmonad `cp scripts/build ~/.xmonad/` and edit script to correct paths
1. Link from `xmonad-afreak` -> `xmonad` somewhere in `$PATH` e.g. `ln -s ~/.local/bin/xmonad-afreak ~/.local/bin/xmonad`
    (not sure why, but `--restart` doesnt work properly if `xmonad` is not in `$PATH`)
1. Append `xmonad` or `xmonad-afreak` to .xinitrc
1. startx
