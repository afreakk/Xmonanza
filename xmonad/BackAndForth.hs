module BackAndForth (backAndForth) where

import XMonad
import qualified XMonad.Hooks.WorkspaceHistory as WH
import XMonad.StackSet

-- needs
--  workspaceHistoryHook
--  attatched to ur loghook

backAndForth :: WorkspaceId -> X ()
backAndForth toWS = do
    cur <- gets (currentTag . windowset)
    if toWS == cur
       then WH.workspaceHistory >>= showPreviousWindow
        else windows (greedyView toWS)

showPreviousWindow :: [WorkspaceId] -> X ()
showPreviousWindow (_:znd:_) = windows (greedyView znd)
showPreviousWindow _         = return ()

