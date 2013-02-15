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

import Blaze.ByteString.Builder
import Control.Applicative
import Control.Lens hiding ((&), (.=))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Char
import Data.Conduit
import Data.Default
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder

-- TODO
import System.Random -- for testCreatePoints
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe
token = unsafePerformIO (BS.init <$> BS.readFile "token")

-- things that ought to be in somebody else's module/package {{{
renderSimpleQueryText b xs = toByteString (renderQueryText b [(x, Just y) | (x, y) <- xs])
urlEncodedBodyText      xs = urlEncodedBody [(encodeUtf8 x, encodeUtf8 y) | (x, y) <- xs]
instance Default Text where def = ""
-- }}}

baseReq segments = def
	{ secure      = True
	, host        = "www.beeminder.com"
	, port        = 443
	, path        = toByteString (encodePathSegments ("api":"v1":segments) <> Builder.fromString ".json")
	, queryString = "?auth_token=" <> token
	}

infixl 4 &
req & q
	| BS.null (queryString req) = req { queryString = renderSimpleQueryText True q }
	| otherwise                 = req { queryString = queryString req <> "&" <> renderSimpleQueryText False q }

-- TODO: make some top-level documentation with these details:
-- * all times are an Integer representing a Unix timestamp
-- * something about requested IDs, like how to request them and how to use them -- can they be used anywhere a normal ID can?

class Resource a where
	_ID        :: Simple Lens a Text
	_UpdatedAt :: Simple Lens a Integer -- ^ you can use this to decide whether or not to use cached information

-- | This is like data-default's 'Default' class, but for types that may not
-- always have a reasonable default for every field. For example, if @Foo@ has
-- such a field, there will be an instance for a function type returning @Foo@
-- or an instance for @IO Foo@ or similar. When a type does have good defaults
-- for all fields, it will instantiate both this and 'Default'.
class Parameters a where with :: a

data UserGoals
	= Slugs  [Text]        -- ^ just the short names (use 'JustTheSlugs')
	| Hashes [Goal]        -- ^ information about all currently existing goals (use 'EverythingCurrent')
	| Diff   [Goal] [Text] -- ^ created or updated goals first, then IDs of deleted goals (use 'Diff')
	deriving (Eq, Ord, Show, Read)

-- | the '_UpdatedAt' value is the upper bound of all updates -- even nested
-- ones to goals, datapoints, etc.
data User = User
	{ username      :: Text
	, timezone      :: Text
	, goals         :: UserGoals
	, userID        :: Text
	, userUpdatedAt :: Integer
	} deriving (Eq, Ord, Show, Read)

instance Resource User where
	_ID        = lens userID        (\s b -> s { userID        = b })
	_UpdatedAt = lens userUpdatedAt (\s b -> s { userUpdatedAt = b })

-- internal type used to get a free list instance when parsing the Diff part of UserGoals
data ID = ID { unID :: Text } deriving (Eq, Ord, Show, Read)
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
	{ userToGet       :: Maybe Text        -- ^ 'Nothing' means \"whoever owns the API token\"
	, goalsFilter     :: Maybe Burner      -- ^ 'Nothing' means \"all goals\"; the 'Front' and 'Back' 'Burner's are the goals above and below the fold in the web interface
	, levelOfDetail   :: LevelOfGoalDetail -- ^ how much information do you want about the user's goals?
	, datapointsCount :: Maybe Integer     -- ^ 'Nothing' means return all data points; 'Just' @n@ will return only the @n@ most recently added (not most recently timestamped!) data points
	} deriving (Eq, Ord, Show, Read)

instance Default LevelOfGoalDetail where def = JustTheSlugs
instance Default UserParameters    where def = UserParameters def def def def

maybeMe :: (a -> Maybe Text) -> (a -> Text)
maybeMe f v = fromMaybe "me" (f v)

textShow, lowerShow :: Show a => a -> Text
textShow  = fromString . show
lowerShow = fromString . map toLower . show

user :: Monad m => UserParameters -> Request m
user p
	= baseReq ["users", maybeMe userToGet p]
	& case levelOfDetail p of
	  	JustTheSlugs      -> []
	  	EverythingCurrent -> [("associations", "true")]
	  	DiffSince t d     -> [("diff_since", lowerShow t), ("skinny", lowerShow d)]
	++ [("goals_filter",     lowerShow b) | Just b <- [goalsFilter     p]]
	++ [("datapoints_count", lowerShow n) | Just n <- [datapointsCount p]]

-- TODO
data Goal = Goal deriving (Eq, Ord, Show, Read)
instance FromJSON Goal where parseJSON _ = return Goal

data Point = Point
	{ timestamp      :: Integer
	, value          :: Double
	, comment        :: Text
	, requestID      :: Maybe Text
	, pointID        :: Text
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
	{ pointsUser :: Maybe Text
	, pointsGoal :: Text
	} deriving (Eq, Ord, Show, Read)

instance goal ~ Text => Parameters (goal -> PointsParameters) where with = PointsParameters def

points :: Monad m => PointsParameters -> Request m
points p = baseReq ["users", maybeMe pointsUser p, "goals", pointsGoal p, "datapoints"]

data PrePoint = PrePoint
	{ preTimestamp :: Integer
	, preValue     :: Double
	, preComment   :: Text
	, preRequestID :: Maybe Text
	} deriving (Eq, Ord, Show, Read)

instance ToJSON PrePoint where
	toJSON p = object $
		[ "timestamp" .= preTimestamp p
		, "value"     .= preValue     p
		, "comment"   .= preComment   p
		] ++
		[ "requestid" .= requestid | Just requestid <- [preRequestID p]]

instance (ts ~ Integer, v ~ Double) => Parameters (ts -> v -> PrePoint) where
	with ts v = PrePoint ts v def def
instance v ~ Double => Parameters (IO (v -> PrePoint)) where
	with = with . round <$> liftIO getPOSIXTime
instance v ~ Double => Parameters (v -> IO PrePoint) where
	with v = ($v) <$> with

-- TODO: need a more scalable and consistent namespacing solution... (that's
-- why we've got the whole "lens" dependency, though, right?)
data CreatePointParameters = CreatePointParameters
	{ createPointUser :: Maybe Text
	, createPointGoal :: Text
	, createPointPre  :: PrePoint
	} deriving (Eq, Ord, Show, Read)

instance (goal ~ Text, ts ~ Integer, v ~ Double) => Parameters (goal -> ts -> v -> CreatePointParameters) where
	with goal ts v = CreatePointParameters def goal (with ts v)
instance (goal ~ Text, v ~ Double) => Parameters (IO (goal -> v -> CreatePointParameters)) where
	with = (\f goal -> CreatePointParameters def goal . f) <$> with
instance (goal ~ Text, v ~ Double) => Parameters (goal -> IO (v -> CreatePointParameters)) where
	with goal = ($goal) <$> with
instance (goal ~ Text, v ~ Double) => Parameters (goal -> v -> IO CreatePointParameters) where
	with goal v = ($v) <$> with goal

data CreatePointsParameters = CreatePointsParameters
	{ createPointsUser :: Maybe Text
	, createPointsGoal :: Text
	, createPointsPre  :: [PrePoint]
	} deriving (Eq, Ord, Show, Read)

instance (goal ~ Text) => Parameters (goal -> CreatePointsParameters) where
	with t = CreatePointsParameters def t def

createPoint , createPointNotify  :: Monad m => CreatePointParameters  -> Request m
createPoints, createPointsNotify :: Monad m => CreatePointsParameters -> Request m

createPointNotify = createPointInternal True
createPoint       = createPointInternal False

createPointInternal sendmail p = urlEncodedBodyText
	([("timestamp", (textShow . preTimestamp . createPointPre) p),
	  ("value"    , (textShow . preValue     . createPointPre) p),
	  ("comment"  , (           preComment   . createPointPre) p)] ++
	 [("requestid", r) | Just r <- [preRequestID (createPointPre p)]] ++
	 [("sendmail" , "true") | sendmail]
	)
	-- TODO: unify with the other occurrence of users/me/goals/goal-name/datapoints
	(baseReq ["users", maybeMe createPointUser p, "goals", createPointGoal p, "datapoints"])

createPointsNotify = createPointsInternal True
createPoints       = createPointsInternal False

createPointsInternal sendmail p = urlEncodedBody
	([("datapoints", toStrict . encode . createPointsPre $ p)] ++
	 [("sendmail"  , "true") | sendmail]
	)
	(baseReq ["users", maybeMe createPointsUser p, "goals", createPointsGoal p, "datapoints", "create_all"])

instance Parameters LevelOfGoalDetail      where with = def
instance Parameters UserParameters         where with = def

testPoly :: FromJSON a => Request (ResourceT IO) -> IO (Maybe a)
testPoly r = do
	man <- newManager def
	bs  <- runResourceT (responseBody <$> httpLbs r {responseTimeout = Nothing} man)
	return (decode bs)

testUser        :: IO (Maybe User)
testPoint       :: IO (Maybe [Point])
testCreatePoint :: IO (Maybe Point)

testUser        = testPoly . user        $   with
testPoint       = testPoly . points      $   with "read-papers"
testCreatePoint = testPoly . createPoint =<< with "apitest" 1

-- TODO: for some reason, the requested ids aren't actually being requested
testCreatePoints :: IO (Maybe [Point])
testCreatePoints = do
	p1 <- with 1
	p2 <- with 1
	n  <- randomIO :: IO Integer
	let params = (with "apitest") { createPointsPre = [p1, p2 { preRequestID = Just (textShow n) }]}
	testPoly (createPoints params)

test = testCreatePoints
