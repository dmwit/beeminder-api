{-# LANGUAGE FlexibleInstances, OverloadedStrings, NoMonomorphismRestriction, TypeFamilies #-}
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
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number
import Data.ByteString (ByteString)
import Data.Char
import Data.Conduit
import Data.Default
import Data.Maybe
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit

-- TODO
import System.IO.Unsafe
token = unsafePerformIO (init `fmap` readFile "token")

server   = "https://www.beeminder.com/"
basePath = "api/v1/"
url p    = server ++ basePath ++ p ++ ".json?auth_token=" ++ token

-- TODO: make some top-level documentation with these details:
-- * all times are an Integer representing a Unix timestamp
-- * something about requested IDs, like how to request them and how to use them -- can they be used anywhere a normal ID can?

class Resource a where
	_ID        :: Simple Lens a String
	_UpdatedAt :: Simple Lens a Integer -- ^ you can use this to decide whether or not to use cached information

-- | This is like data-default's 'Default' class, but for types that may not
-- always have a reasonable default for every field. For example, if @Foo@ has
-- such a field, there will be an instance for a function type returning @Foo@
-- or an instance for @IO Foo@ or similar. When a type does have good defaults
-- for all fields, it will instantiate both this and 'Default'.
class Parameters a where with :: a

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

instance FromJSON Point where
	parseJSON o@(Object v) = Point
		<$> v .: "timestamp"
		<*> v .: "value"
		<*> v .: "comment"
		<*> v .: "requestid"
		<*> v .: "id"
		<*> v .: "updated_at"
	parseJSON o = typeMismatch "datapoint" o

data PointsParameters = PointsParameters
	{ pointsUser :: Maybe String
	, pointsGoal :: String
	} deriving (Eq, Ord, Show, Read)

instance goal ~ String => Parameters (goal -> PointsParameters) where with = PointsParameters def

points :: PointsParameters -> String
points p = url (
	"users/" ++ fromMaybe "me" (pointsUser p) ++ "/" ++
	"goals/" ++ pointsGoal p ++ "/" ++
	"datapoints"
	)

data PrePoint = PrePoint
	{ preTimestamp :: Integer
	, preValue     :: Double
	, preComment   :: String
	, preRequestID :: Maybe String
	} deriving (Eq, Ord, Show, Read)

instance (ts ~ Integer, v ~ Double) => Parameters (ts -> v -> PrePoint) where
	with ts v = PrePoint ts v def def
instance v ~ Double => Parameters (IO (v -> PrePoint)) where
	with = with . round <$> liftIO getPOSIXTime
instance v ~ Double => Parameters (v -> IO PrePoint) where
	with v = ($v) <$> with

-- TODO: need a more scalable and consistent namespacing solution... (that's
-- why we've got the whole "lens" dependency, though, right?)
data CreatePointParameters = CreatePointParameters
	{ createPointUser :: Maybe String
	, createPointGoal :: String
	, createPointPre  :: PrePoint
	} deriving (Eq, Ord, Show, Read)

instance (goal ~ String, ts ~ Integer, v ~ Double) => Parameters (goal -> ts -> v -> CreatePointParameters) where
	with goal ts v = CreatePointParameters def goal (with ts v)
instance (goal ~ String, v ~ Double) => Parameters (IO (goal -> v -> CreatePointParameters)) where
	with = (\f goal -> CreatePointParameters def goal . f) <$> with
instance (goal ~ String, v ~ Double) => Parameters (goal -> IO (v -> CreatePointParameters)) where
	with goal = ($goal) <$> with
instance (goal ~ String, v ~ Double) => Parameters (goal -> v -> IO CreatePointParameters) where
	with goal v = ($v) <$> with goal

data CreatePointsParameters = CreatePointsParameters
	{ createPointsUser :: Maybe String
	, createPointsGoal :: String
	, createPointsPre  :: [PrePoint]
	}

instance Default CreatePointsParameters where def = CreatePointsParameters def def def

createPoint , createPointNotify  :: CreatePointParameters  -> String
createPoints, createPointsNotify :: CreatePointsParameters -> String

-- TODO: test these
createPointNotify p = createPoint p ++ "&sendmail=true"
createPoint p = url ("users/" ++ fromMaybe "me" (createPointUser p) ++ "/goals/" ++ createPointGoal p ++ "/datapoints")
	++ "&timestamp=" ++ (show . preTimestamp . createPointPre) p
	++ "&value="     ++ (show . preValue     . createPointPre) p
	++ "&comment="   ++ (       preComment   . createPointPre) p -- TODO: blatantly wrong (URL encode)
	++ case (preRequestID . createPointPre) p of
		Nothing -> ""
		Just r  -> "&requestid=" ++ r -- TODO: blatantly wrong (URL encode)

-- TODO
createPoints       = undefined
createPointsNotify = undefined

instance Parameters LevelOfGoalDetail      where with = def
instance Parameters UserParameters         where with = def
instance Parameters CreatePointsParameters where with = def

testPoly :: FromJSON a => ByteString -> String -> IO (Maybe a)
testPoly m url = do
	r   <- parseUrl url
	man <- newManager def
	bs  <- runResourceT (responseBody <$> httpLbs r {responseTimeout = Nothing, method = m} man)
	return (decode bs)

testUser  :: IO (Maybe User)
testPoint :: IO (Maybe [Point])
testUser  = testPoly "GET" (user def)
testPoint = testPoly "GET" (points (with "read-papers"))

-- results in a 500 status; perhaps we should finally switch from String to
-- http-conduit's more structured Request type? check out
-- render(Simple)Query in http-types  :Network.HTTP.Types,
-- queryString         in http-conduit:Network.HTTP.Conduit, and
-- urlEncodedBody      in http-conduit:Network.HTTP.Conduit
testCreatePoint :: IO (Maybe Point)
testCreatePoint = testPoly "POST" . createPoint =<< with "testapi" 1

test = testCreatePoint
