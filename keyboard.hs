module Keyboard where

import System.IO

input :: IO Char
input = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    getChar

