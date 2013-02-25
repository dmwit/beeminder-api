import Control.Applicative
import Control.Lens
import Data.Default
import Data.String
import Network.Beeminder
import System.Random

import qualified Data.ByteString as BS

run f p = do
	bs <- BS.readFile "token"
	runBeeminder (BS.init bs) (f p)

testUser         = run user        $ def
testPoints       = run points      $ set _Goal "read-papers" def
testCreatePoint  = run createPoint . set _Goal "apitest" . set _Value 1 =<< now def
-- TODO: for some reason, the requested ids aren't actually being requested
testCreatePoints = do
	p1 <- set _Value 1 <$> now def
	p2 <- set _Value 1 <$> now def
	n  <- randomIO :: IO Integer
	let params = set _PointRequests [p1, set _RequestID (Just . fromString . show $ n) p2] . set _Goal "apitest" $ def
	run createPoints params

test = testCreatePoints
