module Calculator (calculatorPrompt) where
import           XMonad (MonadIO, X)
import           XMonad.Prompt (XPrompt, showXPrompt, completionToCommand, XPConfig, mkXPrompt)
import           XMonad.Util.Run (runProcessWithInput)

data CalculatorMode = CalculatorMode

instance XPrompt CalculatorMode where
    showXPrompt CalculatorMode     = "calc> "
    completionToCommand _ = id

calculatorPrompt :: XPConfig -> X ()
calculatorPrompt c = mkXPrompt CalculatorMode c doCalc (\_ -> return ())

doCalc :: MonadIO m => [Char] -> m [String]
doCalc [] = return []
doCalc s = fmap lines $ fmap stripTab $ runProcessWithInput "calc" [s] ""

stripTab (_:xs) = xs
stripTab [] = []
