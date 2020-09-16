module Time where

import Data.Time


getTime = do
    t <- getCurrentTime
    return $ utctDayTime t
