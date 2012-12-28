{-# LANGUAGE NoMonomorphismRestriction #-}
module Network.Beeminder where

import Network.HTTP.Conduit

-- TODO
import System.IO.Unsafe
token = unsafePerformIO (init `fmap` readFile "dmwit_token")

test = simpleHttp $ "https://www.beeminder.com/api/v1/users/me/goals/early-rising.json?auth_token=" ++ token
