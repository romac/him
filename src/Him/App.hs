
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Him.App where

import           Data.Text (Text)
import qualified Data.Text as T

import qualified System.Console.ANSI as ANSI
import           System.Console.Terminal.Size (Window(..))

import           Data.Functor.Coproduct
import           Control.Monad.FreeC

import           Him.EscapeCode (EscapeCode)
import qualified Him.EscapeCode as EC

data AppF a where
  Exit       ::         AppF ()
  Suspend    ::         AppF ()
  Output     :: Text -> AppF ()
  ReadInput  ::         AppF (Text, Int)
  WindowSize ::         AppF (Maybe (Window Int))

exit :: AppF :<: f => FreeC f ()
exit = injectFreeC Exit

suspend :: AppF :<: f => FreeC f ()
suspend = injectFreeC Suspend

output :: AppF :<: f => Text -> FreeC f ()
output = injectFreeC . Output

output' :: AppF :<: f => String -> FreeC f ()
output' = injectFreeC . Output . T.pack

sendEscapeCode :: AppF :<: f => EscapeCode -> FreeC f ()
sendEscapeCode = output . T.pack . EC.toString

setSGRCode :: AppF :<: f => [ANSI.SGR] -> FreeC f ()
setSGRCode = output' . ANSI.setSGRCode

readInput :: AppF :<: f => FreeC f (Text, Int)
readInput = injectFreeC ReadInput

windowSize :: AppF :<: f => FreeC f (Maybe (Window Int))
windowSize = injectFreeC WindowSize

