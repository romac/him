
module Him.State where

import           Him.Buffer (Buffer)
import qualified Him.Buffer as Buffer

import           Him.Cursor (Cursor)
import qualified Him.Cursor as Cursor

data State = State !Buffer !Cursor
  deriving (Eq, Show)

blank :: State
blank = State Buffer.empty Cursor.blank

