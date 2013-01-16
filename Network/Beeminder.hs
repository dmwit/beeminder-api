{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, StandaloneDeriving #-}
module Network.Beeminder
	( UserGoals(..)
	, User(..)
	, Burner(..)
	, LevelOfGoalDetail(..)
	, UserParameters(..)
	, user
	, Goal(..)
	) where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number
import Data.Char
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

class Resource a where
	_ID        :: Simple Lens a String
	_UpdatedAt :: Simple Lens a Integer -- ^ a Unix timestamp you can use to decide whether or not to use cached information

data UserGoals
	= Slugs  [String]        -- ^ just the short names (use 'JustTheSlugs')
	| Hashes [Goal]          -- ^ information about all currently existing goals (use 'EverythingCurrent')
	| Diff   [Goal] [String] -- ^ created or updated goals first, then IDs of deleted goals (use 'Diff')
	deriving (Eq, Ord, Show, Read)

-- | the '_UpdatedAt' value is the upper bound of all updates -- even nested
-- ones to goals, datapoints, etc.
data User = User
	{ username      :: String
	, timezone      :: String
	, goals         :: UserGoals
	, userID        :: String
	, userUpdatedAt :: Integer
	} deriving (Eq, Ord, Show, Read)

instance Resource User where
	_ID        = lens userID        (\s b -> s { userID        = b })
	_UpdatedAt = lens userUpdatedAt (\s b -> s { userUpdatedAt = b })

-- internal type used to get a free list instance when parsing the Diff part of UserGoals
data ID = ID { unID :: String } deriving (Eq, Ord, Show, Read)
instance FromJSON ID where
	parseJSON (Object v) = ID <$> v .: "id"
	parseJSON o = typeMismatch "ID" o

instance FromJSON UserGoals where
	-- diff comes before hashes so that it is preferred when deleted_goals exists
	-- TODO: this isn't quite right... Diff is clearly differentiable from
	-- Slugs and Hashes, but those two (Slugs and Hashes) aren't differentiable
	-- when the goals list is empty -- need something better than a heuristic here!
	-- possible resolution: have separate User types for each?
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
		<*> parseJSON o
		<*> v .: "id"
		<*> v .: "updated_at"
	parseJSON o = typeMismatch "user object" o

data Burner = Front | Back deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- TODO: list the attributes that you still get with 'skinny' (and test a call with skinny=True)
data LevelOfGoalDetail
	= JustTheSlugs      -- ^ minimal detail: just the "slug" (the part that goes in a URL)
	| EverythingCurrent -- ^ details about all the currently existing goals

	-- the above blank line and the below breech of style are intentional haddock workarounds
	-- | maximal detail: report even about goals that have been deleted
	| DiffSince
		{ since  :: Integer -- ^ a Unix timestamp; show all the changes since that timestamp (new datapoints, deleted goals, etc.)
		, skinny :: Bool    -- ^ when 'True', return only each goal's latest data point and a subset of the attributes for each goal
		}
	deriving (Eq, Ord, Show, Read)
data UserParameters = UserParameters
	{ userToGet       :: Maybe String      -- ^ 'Nothing' means \"whoever owns the API token\"
	, goalsFilter     :: Maybe Burner      -- ^ 'Nothing' means \"all goals\"; the 'Front' and 'Back' 'Burner's are the goals above and below the fold in the web interface
	, levelOfDetail   :: LevelOfGoalDetail -- ^ how much information do you want about the user's goals?
	, datapointsCount :: Maybe Integer     -- ^ 'Nothing' means return all data points; 'Just' @n@ will return only the @n@ most recently added (not most recently timestamped!) data points
	} deriving (Eq, Ord, Show, Read)

instance Default LevelOfGoalDetail where def = JustTheSlugs
instance Default UserParameters    where def = UserParameters def def def def

-- TODO: is String really the right type to use here...? probably ought to return Request m -> Request m or some such thing
-- | the URL to GET for a 'User' object
user :: UserParameters -> String
user p
	=  url ("users/" ++ fromMaybe "me" (userToGet p))
	++ case goalsFilter p of
		Nothing    -> ""
		Just Front -> "&goals_filter=front"
		Just Back  -> "&goals_filter=back"
	++ case levelOfDetail p of
		JustTheSlugs      -> ""
		EverythingCurrent -> "&associations=true"
		DiffSince t d     -> "&diff_since=" ++ show t ++ "&skinny=" ++ map toLower (show d)
	++ case datapointsCount p of
		Nothing -> ""
		Just n  -> "&datapoints_count=" ++ show n

-- TODO
data Goal = Goal deriving (Eq, Ord, Show, Read)
instance FromJSON Goal where parseJSON _ = return Goal

data Point = Point
	{ timestamp      :: Integer
	, value          :: Double
	, comment        :: String
	, requestID      :: Maybe String
	, pointID        :: String
	, pointUpdatedAt :: Integer
	} deriving (Eq, Ord, Show, Read)

instance Resource Point where
	_ID        = lens pointID        (\s b -> s { pointID        = b })
	_UpdatedAt = lens pointUpdatedAt (\s b -> s { pointUpdatedAt = b })

-- TODO: instance FromJSON Point and datapoint access points

test :: IO (Maybe User)
test = do
	r   <- parseUrl (user def)
	man <- newManager def
	bs  <- runResourceT (responseBody <$> httpLbs r {responseTimeout = Nothing} man)
	return (decode bs)
