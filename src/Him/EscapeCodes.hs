
module Him.EscapeCodes where

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

escapeCode :: EscapeCode -> String
escapeCode Clear                = ANSI.clearScreenCode
escapeCode (MoveCursor row col) = ANSI.setCursorPositionCode row col
escapeCode (CursorUp rows)      = ANSI.cursorUpCode rows
escapeCode (CursorDown rows)    = ANSI.cursorDownCode rows
escapeCode (CursorRight cols)   = ANSI.cursorForwardCode cols
escapeCode (CursorLeft cols)    = ANSI.cursorBackwardCode cols
escapeCode (CursorCol col)      = ANSI.setCursorColumnCode col
escapeCode ClearLine            = ANSI.clearLineCode
escapeCode _                    = ""

codeFromStr :: String -> Maybe EscapeCode
codeFromStr "\ESC[A" = Just (CursorUp 1)
codeFromStr "\ESC[B" = Just (CursorDown 1)
codeFromStr "\ESC[C" = Just (CursorRight 1)
codeFromStr "\ESC[D" = Just (CursorLeft 1)
codeFromStr "\ESC[G" = Just (CursorCol 1)
codeFromStr "\ACK"   = Just (CursorRight 1)
codeFromStr "\STX"   = Just (CursorLeft 1)
codeFromStr "\SO"    = Just (CursorDown 1)
codeFromStr "\DLE"   = Just (CursorUp 1)
codeFromStr "\DEL"   = Just Backspace
codeFromStr "\SOH"   = Just StartOfLine
codeFromStr "\ENQ"   = Just EndOfLine
codeFromStr "\r"     = Just NewLine
codeFromStr "\SUB"   = Just Halt
codeFromStr "\ETX"   = Just Abort
codeFromStr _        = Nothing

