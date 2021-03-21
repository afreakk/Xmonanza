module PassFork (
                            -- * Usage
                            -- $usage

                              clipUsernamePrompt
                            , clipPasswordPrompt
                            , passOTPPrompt
                            , passGenerateAndCopyNewPrompt
                            , passGenerateAndCopyExistingPrompt
                            , passRemovePrompt
                            , passEditPrompt
                            , passTypePrompt
                            , passTypeUsername
                            , passAutofillPrompt
                            ) where

import XMonad.Core
import XMonad.Prompt ( XPrompt
                     , showXPrompt
                     , commandToComplete
                     , nextCompletion
                     , getNextCompletion
                     , XPConfig
                     , mkXPrompt
                     , searchPredicate)
import System.Directory (getHomeDirectory)
import System.FilePath (takeExtension, dropExtension, combine)
import System.Posix.Env (getEnv)
import XMonad.Util.Run (runProcessWithInput)
import Utils
import XMonad.Util.Run

type Predicate = String -> String -> Bool

getPassCompl :: [String] -> Predicate -> String -> IO [String]
getPassCompl compls p s = return $ filter (p s) compls

type PromptLabel = String

newtype Pass = Pass PromptLabel

instance XPrompt Pass where
  showXPrompt       (Pass prompt) = prompt ++ ": "
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion

-- | Default password store folder in $HOME/.password-store
--
passwordStoreFolderDefault :: String -> String
passwordStoreFolderDefault home = combine home ".password-store"

-- | Compute the password store's location.
-- Use the PASSWORD_STORE_DIR environment variable to set the password store.
-- If empty, return the password store located in user's home.
--
passwordStoreFolder :: IO String
passwordStoreFolder =
  getEnv "PASSWORD_STORE_DIR" >>= computePasswordStoreDir
  where computePasswordStoreDir Nothing         = fmap passwordStoreFolderDefault getHomeDirectory
        computePasswordStoreDir (Just storeDir) = return storeDir

-- | A pass prompt factory
--
mkPassPrompt :: PromptLabel -> (String -> X ()) -> XPConfig -> X ()
mkPassPrompt promptLabel passwordFunction xpconfig = do
  passwords <- io (passwordStoreFolder >>= getPasswords)
  mkXPrompt (Pass promptLabel) xpconfig (getPassCompl passwords $ searchPredicate xpconfig) passwordFunction

clipPasswordPrompt :: XPConfig -> X ()
clipPasswordPrompt = mkPassPrompt "Select password" clipPassword

clipUsernamePrompt = mkPassPrompt "Select username" clipUsername

-- | A prompt to retrieve a OTP from a given entry.
--
passOTPPrompt :: XPConfig -> X ()
passOTPPrompt = mkPassPrompt "Select OTP" selectOTP

passGenerateAndCopyNewPrompt :: XPConfig -> X ()
passGenerateAndCopyNewPrompt = mkPassPrompt "Generate password" generateAndCopyPasswordForNew

passGenerateAndCopyExistingPrompt :: XPConfig -> X ()
passGenerateAndCopyExistingPrompt = mkPassPrompt "Generate and copy password for existing" generateAndCopyPasswordForExisting

passRemovePrompt :: XPConfig -> X ()
passRemovePrompt = mkPassPrompt "Remove password" removePassword

passTypePrompt :: XPConfig -> X ()
passTypePrompt = mkPassPrompt "Type password" typePassword

passAutofillPrompt :: XPConfig -> X ()
passAutofillPrompt = mkPassPrompt "autofill" typeUsernameAndPassword

passTypeUsername :: XPConfig -> X ()
passTypeUsername = mkPassPrompt "Type username" typeUsername

passEditPrompt :: XPConfig -> X ()
passEditPrompt = mkPassPrompt "Edit password" editPassword

clipPassword :: String -> X ()
clipPassword passLabel = spawn $ "pass show --clip " ++ escapedPassLabel passLabel

clipUsername :: String -> X ()
clipUsername passLabel = spawn $ "pass show " ++ escapedPassLabel passLabel ++ " | " ++ extractUsername ++ " | " ++ stdinToClip

-- | Select a OTP.
--
selectOTP :: String -> X ()
selectOTP passLabel = spawn $ "pass otp --clip " ++ escapedPassLabel passLabel

generateAndCopyPasswordForNew :: String -> X ()
generateAndCopyPasswordForNew passLabel = spawn $ "pass generate -c " ++ escapedPassLabel passLabel ++ " 30"

generateAndCopyPasswordForExisting :: String -> X ()
generateAndCopyPasswordForExisting passLabel = spawn $ "pass generate --in-place -c " ++ escapedPassLabel passLabel ++ " 30"

removePassword :: String -> X ()
removePassword passLabel = spawn $ "pass rm --force " ++ escapedPassLabel passLabel

editPassword :: String -> X ()
editPassword passLabel = runInTerm "" $ "pass edit " ++ escapedPassLabel passLabel

typePassword :: String -> X ()
typePassword passLabel = spawn $ "pass show " ++ escapedPassLabel passLabel ++ "|head -n1|"++ typeWhatsInStdin


typeUsernameAndPassword :: String -> X ()
typeUsernameAndPassword passLabel = spawn $ "IFS= txt=$(pass show " ++ escapedPassLabel passLabel ++ ") && echo $txt |"++extractUsername++"|"++ typeWhatsInStdin ++" && xdotool key Tab && echo $txt |"++extractPassword++"|" ++ typeWhatsInStdin

typeUsername passLabel = spawn $ "pass show " ++ escapedPassLabel passLabel ++ " | " ++ extractUsername ++ " | " ++ typeWhatsInStdin

escapedPassLabel passLabel = "\""++ escapeQuote passLabel ++ "\""

typeWhatsInStdin = "tr -d '\n'|xdotool type --clearmodifiers --file -"
extractUsername = "grep -oP 'username: \\K.*'"
extractPassword = "head -n1"

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = ['\\', '\"']
        escape x = return x

-- | Retrieve the list of passwords from the password store 'passwordStoreDir
getPasswords :: FilePath -> IO [String]
getPasswords passwordStoreDir = do
  files <- runProcessWithInput "find" [
    "-L", -- Traverse symlinks
    passwordStoreDir,
    "-type", "f",
    "-name", "*.gpg",
    "-printf", "%P\n"] []
  return . map removeGpgExtension $ lines files

removeGpgExtension :: String -> String
removeGpgExtension file | takeExtension file == ".gpg" = dropExtension file
                        | otherwise                    = file