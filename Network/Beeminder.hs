{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Network.Beeminder
        ( -- * API calls
          user
        , goal, allGoals
        , createGoal
        , points
        , createPoint , createPointNotify
        , createPoints, createPointsNotify
        , updatePoint , deletePoint

          -- * The Beeminder monad
        , Beeminder
        , Token
        , runBeeminder

          -- * Foo bar
        , UserGoals(..)
        , User(..)
        , Point(..)
        , Goal(..)
        -- UserGoals stuff
        , Burner(..), LevelOfGoalDetail(..)
        -- Goal stuff
        , TimeFrame(..), Aggregate(..), Direction(..), Behavior(..), Target(..), GoalType(..)

          -- * Lenses
        , HasID(..), HasUpdatedAt(..), HasName(..), HasTimezone(..), HasUsername(..), HasGoals(..), HasGoalsFilter(..), HasLevelOfDetail(..)
        , HasPointCount(..), HasTimestamp(..), HasValue(..), HasComment(..), HasRequestID(..), HasGoal(..), HasPointRequest(..), HasPointRequests(..)
        , HasGetPoints(..), HasTitle(..), HasType(..), HasTarget(..), HasBehavior(..), HasPanic(..)

          -- * Utilities
        , now
        , gType
        , externalize
        ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Conduit
import           Data.Default.Class
import           Network.Beeminder.Internal  hiding (allGoals, createGoal,
                                              createPoint, createPointNotify,
                                              createPoints, createPointsNotify,
                                              deletePoint, goal, points,
                                              updatePoint, user)
import qualified Network.Beeminder.Internal  as Internal
import           Network.HTTP.Conduit

data BeeminderEnvironment = BeeminderEnvironment
        { token   :: Token
        , manager :: Manager
        }

type Beeminder_ = MaybeT (ReaderT BeeminderEnvironment (ResourceT IO))
newtype Beeminder a = Beeminder { unBeeminder :: Beeminder_ a }
        deriving (Functor, Applicative, Monad, MonadIO, MonadReader BeeminderEnvironment, MonadThrow, MonadResource, MonadBase IO)

-- The following instance (and the "deriving" clause for MonadThrow,
-- MonadResource, and MonadBase IO) were copied basically
-- verbatim from the "dgs" package, and even there they were written just "by
-- typechecking" rather than with some deep understanding of what's happening.
-- So it wouldn't surprise me if there's bugs here.
instance MonadBaseControl IO Beeminder where
        type StM Beeminder a = Maybe a
        liftBaseWith f = Beeminder $ liftBaseWith $ \g -> f (g . unBeeminder)
        restoreM = Beeminder . restoreM

-- | Run a beeminder computation with the given authentication token,
--   possibly returning a result.
runBeeminder :: Token -> Beeminder a -> IO (Maybe a)
runBeeminder t m = do
        man <- newManager conduitManagerSettings
        runResourceT (runReaderT (runMaybeT (unBeeminder m)) BeeminderEnvironment { token = t, manager = man })

-- | Turn a raw operation taking a token and returning a 'Request'
--   into a nicely encapsulated action in the 'Beeminder' monad.
externalize :: FromJSON a => (Token -> params -> Request) -> params -> Beeminder a
externalize f p = do
        BeeminderEnvironment { token = t, manager = m } <- ask
        r <- httpLbs (f t p) {responseTimeout = responseTimeoutNone} m
        Beeminder . MaybeT . return . decode . responseBody $ r

user        :: UserParameters        -> Beeminder User
goal        :: GoalParameters        -> Beeminder Goal
allGoals    :: AllGoalsParameters    -> Beeminder [Goal]
createGoal  :: CreateGoalParameters  -> Beeminder Goal
points      :: PointsParameters      -> Beeminder [Point]
createPoint , createPointNotify  :: CreatePointParameters  -> Beeminder Point
createPoints, createPointsNotify :: CreatePointsParameters -> Beeminder [Point]
updatePoint :: UpdatePointParameters -> Beeminder Point
deletePoint :: DeletePointParameters -> Beeminder Point

user               = externalize Internal.user
goal               = externalize Internal.goal
allGoals           = externalize Internal.allGoals
createGoal         = externalize Internal.createGoal
points             = externalize Internal.points
createPoint        = externalize Internal.createPoint
createPointNotify  = externalize Internal.createPointNotify
createPoints       = externalize Internal.createPoints
createPointsNotify = externalize Internal.createPointsNotify
updatePoint        = externalize Internal.updatePoint
deletePoint        = externalize Internal.deletePoint
