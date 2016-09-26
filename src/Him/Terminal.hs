module Him.Terminal
  ( withRawInput ) where

import           Control.Exception (finally, catch, Exception, IOException)
import           Data.Typeable (Typeable)
import           System.Posix.IO (stdInput)
import           System.Posix.Terminal (TerminalMode(..), TerminalAttributes(..), TerminalState(..), withoutMode, getTerminalAttributes, setTerminalAttributes)

allTermModes :: [TerminalMode]
allTermModes =
  [ InterruptOnBreak
  , MapCRtoLF
  , IgnoreBreak
  , IgnoreCR
  , IgnoreParityErrors
  , MapLFtoCR
  , CheckParity
  , StripHighBit
  , StartStopInput
  , StartStopOutput
  , MarkParityErrors
  , ProcessOutput
  , LocalMode
  , ReadEnable
  , TwoStopBits
  , HangupOnClose
  , EnableParity
  , OddParity
  , EnableEcho
  , EchoErase
  , EchoKill
  , EchoLF
  , ProcessInput
  , ExtendedFunctions
  , KeyboardInterrupts
  , NoFlushOnInterrupt
  , BackgroundWriteInterrupt
  ]

rawTermSettings :: TerminalAttributes -> TerminalAttributes
rawTermSettings = foldr (.) id funs
  where funs = flip withoutMode <$> allTermModes

setTermAttrsNow settings = setTerminalAttributes stdInput settings Immediately

withRawInput :: IO a -> IO a
withRawInput application = do
  oldTermSettings <- getTerminalAttributes stdInput
  let newTermSettings = rawTermSettings oldTermSettings
  setTermAttrsNow newTermSettings
  application
    `finally` setTermAttrsNow oldTermSettings

