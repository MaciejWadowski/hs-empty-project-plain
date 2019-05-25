module Main where

import StudentApp
--import qualified Data.ByteString.Lazy as B
--import Data.Aeson

main ::  IO ()
main = do
    StudentApp.mainLoop []
    return ()