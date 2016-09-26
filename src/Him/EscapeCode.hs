
module Him.EscapeCode where

import qualified System.Console.ANSI as ANSI

data EscapeCode
  = Clear
  | MoveCursor !Int !Int
  | CursorUp !Int
  | CursorDown !Int
  | CursorRight !Int
  | CursorLeft !Int
  | CursorCol !Int
  | ClearLine
  | Backspace
  | StartOfLine
  | EndOfLine
  | NewLine
  | Abort
  | Halt
  deriving (Eq, Show)

toString :: EscapeCode -> String
toString Clear                = ANSI.clearScreenCode
toString ClearLine            = ANSI.clearLineCode
toString (MoveCursor row col) = ANSI.setCursorPositionCode row col
toString (CursorUp rows)      = ANSI.cursorUpCode rows
toString (CursorDown rows)    = ANSI.cursorDownCode rows
toString (CursorRight cols)   = ANSI.cursorForwardCode cols
toString (CursorLeft cols)    = ANSI.cursorBackwardCode cols
toString (CursorCol col)      = ANSI.setCursorColumnCode col
toString _                    = ""

fromString :: String -> Maybe EscapeCode
fromString "\ESC[A" = Just (CursorUp 1)
fromString "\ESC[B" = Just (CursorDown 1)
fromString "\ESC[C" = Just (CursorRight 1)
fromString "\ESC[D" = Just (CursorLeft 1)
fromString "\ESC[G" = Just (CursorCol 1)
fromString "\ACK"   = Just (CursorRight 1)
fromString "\STX"   = Just (CursorLeft 1)
fromString "\SO"    = Just (CursorDown 1)
fromString "\DLE"   = Just (CursorUp 1)
fromString "\DEL"   = Just Backspace
fromString "\SOH"   = Just StartOfLine
fromString "\ENQ"   = Just EndOfLine
fromString "\r"     = Just NewLine
fromString "\SUB"   = Just Halt
fromString "\ETX"   = Just Abort
fromString _        = Nothing

