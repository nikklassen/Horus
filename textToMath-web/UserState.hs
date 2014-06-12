{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}

module UserState where

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Typeable
import Data.Maybe (fromMaybe)

data User = User { getVars :: Map String String
                 } deriving (Show, Typeable)

data UserDb = UserDb { allUsers :: Map String User
                     } deriving (Typeable)

emptyState :: UserDb
emptyState = UserDb $ Map.fromList []

getUserVars :: String -> Query UserDb (Map String String)
getUserVars userId = do
    db <- ask
    let maybeUser = Map.lookup userId $ allUsers db
    return $ getVars $ fromMaybe emptyUser maybeUser
    where emptyUser = User $ Map.fromList []

setUserVars :: String -> Map String String -> Update UserDb ()
setUserVars userId vars = modify go
    where go (UserDb db) = UserDb $ Map.alter (\_ -> Just $ User vars) userId db

deriveSafeCopy 0 'base ''User
deriveSafeCopy 0 'base ''UserDb
makeAcidic ''UserDb ['getUserVars, 'setUserVars]
