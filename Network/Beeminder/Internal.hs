{-# LANGUAGE OverloadedStrings #-}
module Network.Beeminder.Internal where

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
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock.POSIX
import Data.Universe
import Data.Universe.Helpers
import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder
import qualified Data.ByteString as BS
import qualified Data.Set        as Set
import qualified Data.Vector     as Vector

-- things that ought to be in somebody else's module/package {{{
renderSimpleQueryText b xs = toByteString (renderQueryText b [(x, Just y) | (x, y) <- xs])
urlEncodedBodyText      xs = urlEncodedBody [(encodeUtf8 x, encodeUtf8 y) | (x, y) <- xs]
instance Default Text where def = ""
-- }}}

type Token = ByteString

baseReq token segments = def
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
-- * something about requested IDs, like how to request them and how to use them -- can they be used anywhere a normal ID can? (answer: no, they cannot)

class HasID            a where _ID            :: Simple Lens a Text
class HasUpdatedAt     a where _UpdatedAt     :: Simple Lens a Integer -- ^ you can use this to decide whether or not to use cached information
class HasName          a where _Name          :: Simple Lens a Text
class HasTimezone      a where _Timezone      :: Simple Lens a Text
class HasUsername      a where _Username      :: Simple Lens a (Maybe Text)
class HasGoals         a where _Goals         :: Simple Lens a UserGoals
class HasGoalsFilter   a where _GoalsFilter   :: Simple Lens a (Maybe Burner)
class HasLevelOfDetail a where _LevelOfDetail :: Simple Lens a LevelOfGoalDetail
class HasPointCount    a where _PointCount    :: Simple Lens a (Maybe Integer)
class HasTimestamp     a where _Timestamp     :: Simple Lens a Integer
class HasValue         a where _Value         :: Simple Lens a Double
class HasComment       a where _Comment       :: Simple Lens a Text
class HasRequestID     a where _RequestID     :: Simple Lens a (Maybe Text)
class HasGoal          a where _Goal          :: Simple Lens a Text
class HasPointRequest  a where _PointRequest  :: Simple Lens a PointRequest
class HasPointRequests a where _PointRequests :: Simple Lens a [PointRequest]

data UserGoals
	= Slugs  [Text]        -- ^ just the short names (use 'JustTheSlugs')
	| Hashes [Goal]        -- ^ information about all currently existing goals (use 'EverythingCurrent')
	| Diff   [Goal] [Text] -- ^ created or updated goals first, then IDs of deleted goals (use 'Diff')
	deriving (Eq, Ord, Show, Read)

-- | the '_UpdatedAt' value is the upper bound of all updates -- even nested
-- ones to goals, points, etc.
data User = User
	{ uName      :: Text
	, uTimezone  :: Text
	, uGoals     :: UserGoals
	, uID        :: Text
	, uUpdatedAt :: Integer
	} deriving (Eq, Ord, Show, Read)

instance HasName      User where _Name      = lens uName      (\s b -> s { uName      = b })
instance HasTimezone  User where _Timezone  = lens uTimezone  (\s b -> s { uTimezone  = b })
instance HasGoals     User where _Goals     = lens uGoals     (\s b -> s { uGoals     = b })
instance HasID        User where _ID        = lens uID        (\s b -> s { uID        = b })
instance HasUpdatedAt User where _UpdatedAt = lens uUpdatedAt (\s b -> s { uUpdatedAt = b })

-- internal type used to get a free list instance when parsing the Diff part of UserGoals
data ID = ID { idID :: Text } deriving (Eq, Ord, Show, Read)
instance HasID    ID where _ID = lens idID (\s b -> s { idID = b })
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
		diff   = Diff   <$> v .: "goals" <*> (map idID <$> v .: "deleted_goals")
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
instance FromJSON Burner where parseJSON = showStringChoices "burner" (++"burner")

-- TODO: list the attributes that you still get with 'skinny' (and test a call with skinny=True)
data LevelOfGoalDetail
	= JustTheSlugs      -- ^ minimal detail: just the "slug" (the part that goes in a URL)
	| EverythingCurrent -- ^ details about all the currently existing goals

	-- the above blank line and the below breech of style are intentional haddock workarounds
	-- | maximal detail: report even about goals that have been deleted
	| DiffSince
		{ since  :: Integer -- ^ a Unix timestamp; show all the changes since that timestamp (new points, deleted goals, etc.)
		, skinny :: Bool    -- ^ when 'True', return only each goal's latest data point and a subset of the attributes for each goal
		}
	deriving (Eq, Ord, Show, Read)

instance Default LevelOfGoalDetail where def = JustTheSlugs

data UserParameters = UserParameters
	{ upUsername      :: Maybe Text        -- ^ 'Nothing' means \"whoever owns the API token\"
	, upGoalsFilter   :: Maybe Burner      -- ^ 'Nothing' means \"all goals\"; the 'Front' and 'Back' 'Burner's are the goals above and below the fold in the web interface
	, upLevelOfDetail :: LevelOfGoalDetail -- ^ how much information do you want about the user's goals?
	, upPointCount    :: Maybe Integer     -- ^ 'Nothing' means return all data points; 'Just' @n@ will return only the @n@ most recently added (not most recently timestamped!) data points
	} deriving (Eq, Ord, Show, Read)

instance Default UserParameters where def = UserParameters def def def def

-- TODO: look into using Control.Lens.TH.makeFields to reduce this boilerplate
instance HasUsername      UserParameters where _Username      = lens upUsername      (\s b -> s { upUsername      = b })
instance HasGoalsFilter   UserParameters where _GoalsFilter   = lens upGoalsFilter   (\s b -> s { upGoalsFilter   = b })
instance HasLevelOfDetail UserParameters where _LevelOfDetail = lens upLevelOfDetail (\s b -> s { upLevelOfDetail = b })
instance HasPointCount    UserParameters where _PointCount    = lens upPointCount    (\s b -> s { upPointCount    = b })

maybeMe :: HasUsername a => a -> Text
maybeMe v = fromMaybe "me" (view _Username v)

textShow, lowerShow :: Show a => a -> Text
textShow  = fromString . show
lowerShow = fromString . map toLower . show

user :: Monad m => Token -> UserParameters -> Request m
user t p
	= baseReq t ["users", maybeMe p]
	& case view _LevelOfDetail p of
	  	JustTheSlugs      -> []
	  	EverythingCurrent -> [("associations", "true")]
	  	DiffSince t d     -> [("diff_since", lowerShow t), ("skinny", lowerShow d)]
	++ [("goals_filter",     lowerShow b) | Just b <- [view _GoalsFilter p]]
	++ [("datapoints_count", lowerShow n) | Just n <- [view _PointCount  p]]

data Timeframe = Year | Month | Week | Day | Hour deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Aggregate = Last | First | Min | Max | Mean  deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Direction = Up | Down                        deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance FromJSON Timeframe where parseJSON = showStringChoices "timeframe" (take 1)
instance FromJSON Aggregate where parseJSON = showStringChoices "aggregate" id
instance FromJSON Direction where
	parseJSON (Number (I   1 )) = return Up
	parseJSON (Number (I (-1))) = return Down
	parseJSON v = typeMismatch "direction (either 1 or -1)" v

showStringChoices s f = stringChoices s [(f . map toLower . show $ a, a) | a <- universeF]
stringChoices s cs_ v =
	case v of
		String t -> case lookup t cs of
			Just r  -> return r
			Nothing -> wrong
		_ -> wrong
	where
	cs  = [(fromString n, r) | (n, r) <- cs_]
	ns  = map fst cs
	ns' = init ns
	n   = last ns
	wrong         = typeMismatch errorMessage v
	errorMessage  = s <> " (" <> errorInternal <> ")"
	errorInternal = case ns of
		[]      -> "there are no values of this type!"
		[n]     -> "must be " <> show n
		[n, n'] -> "either " <> show n <> " or " <> show n'
		_       -> "one of " <> intercalate "," (map show ns') <> ", or " <> show n

data Behavior
	= Exponential   -- ^ interpret rate as multiplicative rather than additive
	| Cumulative    -- ^ plot values as the sum of the points
	| Odometer      -- ^ treat zero as an odomoter reset rather than a literal 0
	| Edgy          -- ^ initial data point goes at the road edge (not center)
	| Noisy         -- ^ use points (not just rate) when computing road width
	| StepLine      -- ^ use steppy-like line when rendering the graph
	| Rosy          -- ^ show the optimistic rosy dots when rendering the graph
	| MovingAverage -- ^ graph the moving average
	| Aura          -- ^ render the turquoise confidence area
	| Ephemeral     -- ^ garbage collect this goal after a bit
	| Secret        -- ^ only the owner can see the goal
	| SecretPoints  -- ^ only the owner can see the points
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

parseBehaviorSet v = do
	behaviors <- mapM (parseBehavior v) universeF
	return (Set.fromList [b | (b, h) <- behaviors, polarity b h])
	where
	parseBehavior v b = (,) b <$> (v .: fieldName b)
	fieldName Exponential   = "exprd"
	fieldName Cumulative    = "kyoom"
	fieldName Odometer      = "odom"
	fieldName Edgy          = "edgy"
	fieldName Noisy         = "noisy"
	fieldName StepLine      = "steppy"
	fieldName Rosy          = "rosy"
	fieldName MovingAverage = "movingav"
	fieldName Aura          = "aura"
	fieldName Ephemeral     = "ephem"
	fieldName Secret        = "secret"
	fieldName SecretPoints  = "datapublic"
	polarity  SecretPoints  = not
	polarity  _             = id

data Target
	= MissingDate  {                   tValue :: Double, tRate :: Double }
	| MissingValue { tDate :: Integer,                   tRate :: Double }
	| MissingRate  { tDate :: Integer, tValue :: Double                  }
	deriving (Eq, Ord, Show, Read)

instance FromJSON Target where
	parseJSON v@(Array vs) = case Vector.toList vs of
		[Null        , Number v, Number r] -> return $ MissingDate    (toDouble v) (toDouble r)
		[Number (I t), Null    , Number r] -> return $ MissingValue t              (toDouble r)
		[Number (I t), Number v, Null    ] -> return $ MissingRate  t (toDouble v)
		_ -> typeMismatch "target: two out of three values of [goal date,value,rate]" v
		where
		toDouble (I n) = fromInteger n
		toDouble (D n) = n
	parseJSON v = typeMismatch "target (array)" v

-- TODO: Goals don't match the spec: they have an "id", "graphsum", and "rah"
-- fields. I wonder what they're for!
data Goal = Goal
	{ gID               :: Text
	, gSlug             :: Text
	, gUpdatedAt        :: Integer
	, gBurner           :: Burner
	, gTitle            :: Text
	, gDate             :: Integer                   -- ^ of completion
	, gValue            :: Double
	, gRate             :: Double
	, gRatePeriod       :: Timeframe
	, gGraph            :: Text                      -- ^ URL of graph image TODO: can this be computed from gID?
	, gThumb            :: Text                      -- ^ URL of graph thumb TODO: can this be computed from gID?
	, gLoseDate         :: Integer                   -- ^ assuming no more data reported
	, gPanic            :: Double                    -- ^ how many seconds before 'gLoseDate' to FREAK OUT
	, gQueued           :: Bool                      -- ^ is graph still rendering?
	, gPoints           :: [Point]                   -- ^ empty unless you explicitly ask for it not to be
	, gPointCount       :: Integer
	, gPledge           :: Double                    -- TODO: Integer? (and change gStepdownSchedule to match)
	, gStartDate        :: Integer
	, gStartValue       :: Double
	, gCurrentDate      :: Integer
	, gCurrentValue     :: Double
	, gReportedDate     :: Integer
	, gYaw              :: Direction                 -- ^ which side of the road is good?
	, gSlope            :: Direction                 -- ^ which way is the road going? TODO: can this be inferred from gRate? (what happens when a downward-sloping graph gets dialed to an upward slope?)
	, gLane             :: Integer
	, gMathIsHard       :: (Integer, Double, Double) -- ^ date, value, and rate??? TODO: can this be inferred from gDate, gValue, gRate? if not, what's the difference??
	, gSummary          :: (Text, Text, Text)        -- headsum, limsum, graphsum
	, gWon              :: Bool                      -- TODO: can this be inferred from current date, gDate, and gLoseDate?
	, gFrozen           :: Bool                      -- TODO: how does this differ from gLost...?
	, gLost             :: Bool                      -- TODO: can this be inferred from current date, gDate, and gLoseDate? (what happens in the grace period?)
	, gStepdownSchedule :: Maybe (Double, Integer)   -- ^ the current pledge (TODO: can this be inferred from gPledge?) and the date of a scheduled future stepdown, if any
	, gRoad             :: [Target]
	, gAggregate        :: Aggregate                 -- ^ what to do with multiple points on a given day
	, gBehavior         :: Set Behavior
	} deriving (Eq, Ord, Show, Read)

-- TODO: lens instances for Goal

-- TODO, obviously
instance FromJSON Goal where
	parseJSON o@(Object v) = Goal
		<$> v .: "id"
		<*> v .: "slug"
		<*> v .: "updated_at"
		<*> v .: "burner"
		<*> v .: "title"
		<*> v .: "goaldate"
		<*> v .: "goalvalue"
		<*> v .: "rate"
		<*> v .: "runits"
		<*> v .: "graph_url"
		<*> v .: "thumb_url"
		<*> v .: "losedate"
		<*> v .: "panic"
		<*> v .: "queued"
		<*> v .: "datapoints"
		<*> v .: "numpts"
		<*> v .: "pledge"
		<*> v .: "initday"
		<*> v .: "initval"
		<*> v .: "curday"
		<*> v .: "curval"
		<*> v .: "lastday"
		<*> v .: "yaw"
		<*> v .: "dir"
		<*> v .: "lane"
		<*> v .: "mathishard"
		<*> v .: "headsum,limsum,graphsum" -- TODO
		<*> v .: "won"
		<*> v .: "frozen"
		<*> v .: "lost"
		<*> v .: "contract" -- TODO: gStepdownSchedule :: Maybe (Double, Integer)   -- ^ the current pledge (TODO: can this be inferred from gPledge?) and the date of a scheduled future stepdown, if any
		<*> v .: "road"
		<*> v .: "aggday"
		<*> parseBehaviorSet v

data Point = Point
	{ pTimestamp :: Integer
	, pValue     :: Double
	, pComment   :: Text
	, pRequestID :: Maybe Text
	, pID        :: Text
	, pUpdatedAt :: Integer
	} deriving (Eq, Ord, Show, Read)

instance HasTimestamp Point where _Timestamp = lens pTimestamp (\s b -> s { pTimestamp = b })
instance HasValue     Point where _Value     = lens pValue     (\s b -> s { pValue     = b })
instance HasComment   Point where _Comment   = lens pComment   (\s b -> s { pComment   = b })
instance HasRequestID Point where _RequestID = lens pRequestID (\s b -> s { pRequestID = b })
instance HasID        Point where _ID        = lens pID        (\s b -> s { pID        = b })
instance HasUpdatedAt Point where _UpdatedAt = lens pUpdatedAt (\s b -> s { pUpdatedAt = b })

instance FromJSON Point where
	parseJSON o@(Object v) = Point
		<$> v .: "timestamp"
		<*> v .: "value"
		<*> v .: "comment"
		<*> v .: "requestid"
		<*> v .: "id"
		<*> v .: "updated_at"
	parseJSON o = typeMismatch "point" o

-- | You will not like the '_Goal' you get from the 'Default' instance.
data PointsParameters = PointsParameters
	{ ppUsername :: Maybe Text
	, ppGoal     :: Text
	} deriving (Eq, Ord, Show, Read)

instance Default PointsParameters where def = PointsParameters def def

instance HasUsername PointsParameters where _Username = lens ppUsername (\s b -> s { ppUsername = b })
instance HasGoal     PointsParameters where _Goal     = lens ppGoal     (\s b -> s { ppGoal     = b })

points :: Monad m => Token -> PointsParameters -> Request m
points t p = baseReq t ["users", maybeMe p, "goals", view _Goal p, "datapoints"]

-- | You will not like the '_Timestamp' or '_Value' you get from the
-- 'Default' instance. You may like 'now'.
data PointRequest = PointRequest
	{ prTimestamp :: Integer
	, prValue     :: Double
	, prComment   :: Text
	, prRequestID :: Maybe Text
	} deriving (Eq, Ord, Show, Read)

instance Default PointRequest where def = PointRequest def def def def

instance HasTimestamp PointRequest where _Timestamp = lens prTimestamp (\s b -> s { prTimestamp = b })
instance HasValue     PointRequest where _Value     = lens prValue     (\s b -> s { prValue     = b })
instance HasComment   PointRequest where _Comment   = lens prComment   (\s b -> s { prComment   = b })
instance HasRequestID PointRequest where _RequestID = lens prRequestID (\s b -> s { prRequestID = b })

instance ToJSON PointRequest where
	toJSON p = object $
		[ "timestamp" .= view _Timestamp p
		, "value"     .= view _Value     p
		, "comment"   .= view _Comment   p
		] ++
		[ "requestid" .= requestid | Just requestid <- [view _RequestID p]]

-- | Set the timestamp to the current time.
now :: (MonadIO m, HasTimestamp a) => a -> m a
now a = liftIO $ flip (set _Timestamp) a . round <$> getPOSIXTime

-- TODO: perhaps we shouldn't have separate createPoint and createPoints! After
-- all, the latter completely subsumes the former, and we can internally check
-- the length of the list to decide what to do if the single-point creation API
-- call turns out to be better for some reason.

-- | You will not like the '_Goal', '_Timestamp', or '_Value' you get from the
-- 'Default' instance. You may like 'now'.
data CreatePointParameters = CreatePointParameters
	{ cppUsername     :: Maybe Text
	, cppGoal         :: Text
	, cppPointRequest :: PointRequest
	} deriving (Eq, Ord, Show, Read)

instance Default CreatePointParameters where def = CreatePointParameters def def def

instance HasUsername     CreatePointParameters where _Username     = lens cppUsername     (\s b -> s { cppUsername     = b })
instance HasGoal         CreatePointParameters where _Goal         = lens cppGoal         (\s b -> s { cppGoal         = b })
instance HasPointRequest CreatePointParameters where _PointRequest = lens cppPointRequest (\s b -> s { cppPointRequest = b })
instance HasTimestamp    CreatePointParameters where _Timestamp    = _PointRequest . _Timestamp
instance HasValue        CreatePointParameters where _Value        = _PointRequest . _Value
instance HasComment      CreatePointParameters where _Comment      = _PointRequest . _Comment
instance HasRequestID    CreatePointParameters where _RequestID    = _PointRequest . _RequestID

-- | You will not like the '_Goal' or '_PointRequests' you get from the
-- 'Default' instance.
data CreatePointsParameters = CreatePointsParameters
	{ cpspUsername      :: Maybe Text
	, cpspGoal          :: Text
	, cpspPointRequests :: [PointRequest]
	} deriving (Eq, Ord, Show, Read)

instance Default CreatePointsParameters where def = CreatePointsParameters def def def

instance HasUsername      CreatePointsParameters where _Username      = lens cpspUsername      (\s b -> s { cpspUsername      = b })
instance HasGoal          CreatePointsParameters where _Goal          = lens cpspGoal          (\s b -> s { cpspGoal          = b })
instance HasPointRequests CreatePointsParameters where _PointRequests = lens cpspPointRequests (\s b -> s { cpspPointRequests = b })

createPoint , createPointNotify  :: Monad m => Token -> CreatePointParameters  -> Request m
createPoints, createPointsNotify :: Monad m => Token -> CreatePointsParameters -> Request m

createPointNotify = createPointInternal True
createPoint       = createPointInternal False

tsvcArgs p =
	[ ("timestamp", (textShow . view _Timestamp) p)
	, ("value"    , (textShow . view _Value    ) p)
	, ("comment"  , (           view _Comment  ) p)
	]

createPointInternal sendmail t p = urlEncodedBodyText
	(  tsvcArgs p
	++ [("requestid", r) | Just r <- [view _RequestID p]]
	++ [("sendmail" , "true") | sendmail]
	)
	-- TODO: unify with the other occurrence of users/me/goals/goal-name/datapoints
	(baseReq t ["users", maybeMe p, "goals", view _Goal p, "datapoints"])

createPointsNotify = createPointsInternal True
createPoints       = createPointsInternal False

createPointsInternal sendmail t p = urlEncodedBody
	([("datapoints", toStrict . encode . view _PointRequests $ p)] ++
	 [("sendmail"  , "true") | sendmail]
	)
	(baseReq t ["users", maybeMe p, "goals", view _Goal p, "datapoints", "create_all"])

-- | You will not like the '_Goal', '_ID', '_Timestamp', or '_Value' you get
-- from the 'Default' instance. You may like 'now'.
data UpdatePointParameters = UpdatePointParameters
	{ uppUsername  :: Maybe Text
	, uppGoal      :: Text
	, uppID        :: Text
	, uppTimestamp :: Integer
	, uppValue     :: Double
	, uppComment   :: Text
	} deriving (Eq, Ord, Show, Read)

instance Default UpdatePointParameters where def = UpdatePointParameters def def def def def def

instance HasUsername  UpdatePointParameters where _Username  = lens uppUsername  (\s b -> s { uppUsername  = b })
instance HasGoal      UpdatePointParameters where _Goal      = lens uppGoal      (\s b -> s { uppGoal      = b })
instance HasID        UpdatePointParameters where _ID        = lens uppID        (\s b -> s { uppID        = b })
instance HasTimestamp UpdatePointParameters where _Timestamp = lens uppTimestamp (\s b -> s { uppTimestamp = b })
instance HasValue     UpdatePointParameters where _Value     = lens uppValue     (\s b -> s { uppValue     = b })
instance HasComment   UpdatePointParameters where _Comment   = lens uppComment   (\s b -> s { uppComment   = b })

updatePoint :: Monad m => Token -> UpdatePointParameters -> Request m
updatePoint t p = (urlEncodedBodyText
	(tsvcArgs p)
	(baseReq t ["users", maybeMe p, "goals", view _Goal p, "datapoints", view _ID p])
	) { method = "PUT" }

-- TODO: is there some commonality that we can pull out from this and other parameters data types??
-- | You will not like the '_Goal' or '_ID' you get from the 'Default'
-- instance.
data DeletePointParameters = DeletePointParameters
	{ dppUsername :: Maybe Text
	, dppGoal     :: Text
	, dppID       :: Text
	} deriving (Eq, Ord, Show, Read)

instance Default DeletePointParameters where def = DeletePointParameters def def def

instance HasUsername DeletePointParameters where _Username = lens dppUsername (\s b -> s { dppUsername = b })
instance HasGoal     DeletePointParameters where _Goal     = lens dppGoal     (\s b -> s { dppGoal     = b })
instance HasID       DeletePointParameters where _ID       = lens dppID       (\s b -> s { dppID       = b })

deletePoint :: Monad m => Token -> DeletePointParameters -> Request m
deletePoint t p = (baseReq t ["users", maybeMe p, "goals", view _Goal p, "datapoints", view _ID p]) { method = "DELETE" }
-- Finite instances {{{
instance Universe Burner    where universe = universeDef
instance Universe Timeframe where universe = universeDef
instance Universe Aggregate where universe = universeDef
instance Universe Direction where universe = universeDef
instance Universe Behavior  where universe = universeDef
instance Finite Burner
instance Finite Timeframe
instance Finite Aggregate
instance Finite Direction
instance Finite Behavior
-- }}}
