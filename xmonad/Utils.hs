module Utils (stdinToClip) where

stdinToClip = "xclip -in -selection primary -f | xclip -in -selection clipboard"
