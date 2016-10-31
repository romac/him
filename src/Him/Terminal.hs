
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Him.Terminal
  ( withRawInput
  ) where

import           Control.Exception (finally)
import           System.Posix.IO (stdInput)
import           System.Posix.Terminal
                   ( TerminalMode(..), TerminalAttributes, TerminalState(..)
                   , withoutMode, getTerminalAttributes, setTerminalAttributes
                   )

-- import           GHC.Generics
-- import           Generics.Deriving.Enum

-- deriving instance Generic TerminalMode
-- deriving instance GEnum TerminalMode

-- allTermModes :: [TerminalMode]
-- allTermModes = genum

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

setTerminalAttributesNow :: TerminalAttributes -> IO ()
setTerminalAttributesNow attrs = setTerminalAttributes stdInput attrs Immediately

withRawInput :: IO a -> IO a
withRawInput application = do
  oldTermSettings <- getTerminalAttributes stdInput
  let newTermSettings = rawTermSettings oldTermSettings
  setTerminalAttributesNow newTermSettings
  application
    `finally` setTerminalAttributesNow oldTermSettings

