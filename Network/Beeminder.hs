{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, StandaloneDeriving #-}
module Network.Beeminder where

import Control.Applicative
import Data.Aeson
import Data.Attoparsec.Number
import Network.HTTP.Conduit

-- TODO
import System.IO.Unsafe
token = unsafePerformIO (init `fmap` readFile "token")

server   = "https://www.beeminder.com/"
basePath = "api/v1/"
url p    = server ++ basePath ++ p ++ ".json?auth_token=" ++ token

deriving instance Read Number

data UserGoals
	= Slugs  [String]
	| Hashes [()]
	| Diff   [()] [()]
	deriving (Eq, Ord, Show, Read)

data User = User
	{ username  :: String
	, timezone  :: String
	, updatedAt :: Number
	, goals     :: UserGoals
	} deriving (Eq, Ord, Show, Read)

instance FromJSON UserGoals where
	parseJSON v = Slugs <$> parseJSON v -- TODO

-- TODO: the implementation doesn't match the spec: it has "id" and
-- "has_authorized_fitbit" fields. I wonder what they're for!
instance FromJSON User where
	parseJSON (Object v) = User
		<$> v .: "username"
		<*> v .: "timezone"
		<*> v .: "updated_at"
		<*> v .: "goals"

test :: IO (Maybe User)
test = decode <$> simpleHttp (url "users/me")
