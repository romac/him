
module Him.Cursor where

data Cursor = Cursor !Int !Int
  deriving (Eq, Show)

blank :: Cursor
blank = Cursor 0 0

up    n (Cursor row col) = Cursor (row - n) col
down  n (Cursor row col) = Cursor (row + n) col
left  n (Cursor row col) = Cursor row (col - n)
right n (Cursor row col) = Cursor row (col + n)
col   n (Cursor row _)   = Cursor row n
row   n (Cursor _ col)   = Cursor n col

