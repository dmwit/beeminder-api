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
	| Hashes [Goal]
	| Diff   [Goal] [Goal]
	deriving (Eq, Ord, Show, Read)

data User = User
	{ username  :: String
	, timezone  :: String
	, updatedAt :: Number
	, goals     :: UserGoals
	} deriving (Eq, Ord, Show, Read)

instance FromJSON UserGoals where
	-- diff comes before hashes so that it is preferred when deleted_goals exists
	parseJSON (Object v) = slugs <|> diff <|> hashes where
		slugs  = Slugs  <$> v .: "goals"
		hashes = Hashes <$> v .: "goals"
		diff   = Diff   <$> v .: "goals" <*> v .: "deleted_goals"

-- TODO: the implementation doesn't match the spec: it has "id" and
-- "has_authorized_fitbit" fields. I wonder what they're for!
instance FromJSON User where
	parseJSON o@(Object v) = User
		<$> v .: "username"
		<*> v .: "timezone"
		<*> v .: "updated_at"
		<*> parseJSON o

type Goal = () -- TODO

test :: IO (Maybe User)
test = decode <$> simpleHttp (url "users/me")
