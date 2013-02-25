module Network.Beeminder
	-- TODO: this export list is hopelessly incomplete
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
import Data.Aeson -- TODO: maybe don't need this in the end?
import Data.Conduit
import Data.Default
import Network.HTTP.Conduit
import Network.Beeminder.Internal

-- TODO
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe
import System.Random -- for testCreatePoints
token = unsafePerformIO (BS.init <$> BS.readFile "token")

testPoly :: FromJSON a => Request (ResourceT IO) -> IO (Maybe a)
testPoly r = do
	man <- newManager def
	bs  <- runResourceT (responseBody <$> httpLbs r {responseTimeout = Nothing} man)
	return (decode bs)

testUser        :: IO (Maybe User)
testPoint       :: IO (Maybe [Point])
testCreatePoint :: IO (Maybe Point)

testUser        = testPoly . user        token $ def
testPoint       = testPoly . points      token . set _Goal "read-papers" $ def
testCreatePoint = testPoly . createPoint token . set _Goal "apitest"     . set _Value 1 =<< now def

-- TODO: for some reason, the requested ids aren't actually being requested
testCreatePoints :: IO (Maybe [Point])
testCreatePoints = do
	p1 <- set _Value 1 <$> now def
	p2 <- set _Value 1 <$> now def
	n  <- randomIO :: IO Integer
	let params = set _PointRequests [p1, set _RequestID (Just (textShow n)) p2] . set _Goal "apitest" $ def
	testPoly (createPoints token params)

test = testCreatePoints
