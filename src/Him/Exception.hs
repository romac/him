
module Him.Exception where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data AppException = AppException !String
  deriving (Show, Typeable)

instance Exception AppException

