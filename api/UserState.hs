{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}

module UserState where

import Calculator.Data.AST (AST(..))
import Calculator.Data.Decimal ()
import Calculator.Data.Env (UserPrefs(..), defaultPrefs)
import Calculator.Data.Function (Function(..))
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as Map

data User = User { variables :: Map String AST
                 , functions :: Map String (Function, String)
                 , prefs :: UserPrefs
                 } deriving (Show, Typeable)

data UserDb = UserDb { allUsers :: Map String User
                     } deriving (Typeable)

emptyState :: UserDb
emptyState = UserDb Map.empty

newUser :: User
newUser = User Map.empty Map.empty defaultPrefs

getUser :: String -> Query UserDb User
getUser userId = do
    db <- ask
    let maybeUser = Map.lookup userId $ allUsers db
    return $ fromMaybe newUser maybeUser

setUser :: String -> User -> Update UserDb ()
setUser userId user = modify go
    where go (UserDb db) = UserDb $ Map.alter (\_ -> Just user) userId db

deriveSafeCopy 0 'base ''User
deriveSafeCopy 0 'base ''UserDb
makeAcidic ''UserDb ['getUser, 'setUser]
