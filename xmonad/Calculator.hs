module Calculator (calculatorPrompt) where
import           XMonad (MonadIO, X)
import           XMonad.Prompt (XPrompt, showXPrompt, completionToCommand, XPConfig, mkXPrompt)
import           XMonad.Util.Run (runProcessWithInput)
import XMonad
import Data.Maybe
import Data.Functor

data CalculatorMode = CalculatorMode

instance XPrompt CalculatorMode where
    showXPrompt CalculatorMode     = "calc> "
    completionToCommand _ = id

maybeValueToClipboard :: Show a => Maybe a -> X ()
maybeValueToClipboard (Just v) = spawn ("echo -n " ++ (show v) ++ " | xclip -in -selection primary -f | xclip -in -selection clipboard &> /dev/null")
maybeValueToClipboard Nothing  = return ()

calculatorPrompt :: XPConfig -> X ()
calculatorPrompt c = mkXPrompt CalculatorMode c doCalc (\e -> (doCalc e) <&> listToMaybe >>= maybeValueToClipboard)

doCalc :: MonadIO m => [Char] -> m [String]
doCalc [] = return []
doCalc s = fmap lines $ fmap stripTab $ runProcessWithInput "calc" [s] ""

stripTab (_:xs) = xs
stripTab [] = []
