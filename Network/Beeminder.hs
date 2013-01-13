{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, StandaloneDeriving #-}
module Network.Beeminder where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number
import Data.Conduit
import Data.Default
import Data.Maybe
import Network.HTTP.Conduit

-- TODO
import System.IO.Unsafe
token = unsafePerformIO (init `fmap` readFile "token")

server   = "https://www.beeminder.com/"
basePath = "api/v1/"
url p    = server ++ basePath ++ p ++ ".json?auth_token=" ++ token

data UserGoals
	= Slugs  [String]
	| Hashes [Goal]
	| Diff   [Goal] [String] -- ^ created or updated goals first, then IDs of deleted goals
	deriving (Eq, Ord, Show, Read)

data User = User
	{ username  :: String
	, timezone  :: String
	, updatedAt :: Integer
	, goals     :: UserGoals
	} deriving (Eq, Ord, Show, Read)

-- internal type used to get a free list instance when parsing the Diff part of UserGoals
data ID = ID { unID :: String } deriving (Eq, Ord, Show, Read)
instance FromJSON ID where
	parseJSON (Object v) = ID <$> v .: "id"
	parseJSON o = typeMismatch "ID" o

instance FromJSON UserGoals where
	-- diff comes before hashes so that it is preferred when deleted_goals exists
	parseJSON (Object v) = slugs <|> diff <|> hashes where
		slugs  = Slugs  <$> v .: "goals"
		hashes = Hashes <$> v .: "goals"
		diff   = Diff   <$> v .: "goals" <*> (map unID <$> v .: "deleted_goals")
	parseJSON o = typeMismatch "hash with goals (either a list of slugs or a list of goal objects)" o

-- TODO: the implementation doesn't match the spec: it has "id" and
-- "has_authorized_fitbit" fields. I wonder what they're for!
instance FromJSON User where
	parseJSON o@(Object v) = User
		<$> v .: "username"
		<*> v .: "timezone"
		<*> v .: "updated_at"
		<*> parseJSON o
	parseJSON o = typeMismatch "user object" o

data Burner = Front | Back deriving (Eq, Ord, Show, Read, Bounded, Enum)
data DiffParameters = DiffParameters
	{ since  :: Integer
	, skinny :: Bool
	} deriving (Eq, Ord, Show, Read)
data UserParameters = UserParameters
	{ userToGet       :: Maybe String
	, goalsFilter     :: Maybe Burner
	, associations    :: Bool
	, diffSince       :: Maybe DiffParameters
	, datapointsCount :: Maybe Integer
	} deriving (Eq, Ord, Show, Read)

instance Default DiffParameters where def = DiffParameters def False
instance Default UserParameters where def = UserParameters def def False def def

-- TODO: is String really the right type to use here...? probably ought to return Request m -> Request m or some such thing
user :: UserParameters -> String
user p
	=  url ("users/" ++ fromMaybe "me" (userToGet p))
	++ case goalsFilter p of
		Nothing -> ""
		Just Front -> "&goals_filter=front"
		Just Back  -> "&goals_filter=back"
	-- TODO: figure out a good style + add all the other parameters

-- TODO
data Goal = Goal deriving (Eq, Ord, Show, Read)
instance FromJSON Goal where parseJSON _ = return Goal

test :: IO (Maybe User)
test = do
	r   <- parseUrl (url "users/me" ++ "&diff_since=0") -- oh yes, this can be done much more principledly
	man <- newManager def
	bs  <- runResourceT (responseBody <$> httpLbs r {responseTimeout = Nothing} man)
	return (decode bs)
