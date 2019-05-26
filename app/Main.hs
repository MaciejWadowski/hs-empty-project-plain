module Main where

import Db
--import qualified Data.ByteString.Lazy as B
--import Data.Aeson

main ::  IO ()
main = do
    Db.main'
    return ()