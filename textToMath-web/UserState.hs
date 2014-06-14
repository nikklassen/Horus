{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UserState where

import Calculator.Data.AST (AST(..))
import Calculator.Functions (Function(..))

import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Typeable
import Data.Maybe (fromMaybe)

data User = User { getVars :: Map String String
                 , getFuncs :: Map String Function
                 } deriving (Show, Typeable)

data UserDb = UserDb { allUsers :: Map String User
                     } deriving (Typeable)

emptyState :: UserDb
emptyState = UserDb Map.empty

getUser :: String -> Query UserDb User
getUser userId = do
    db <- ask
    let maybeUser = Map.lookup userId $ allUsers db
    return $ fromMaybe emptyUser maybeUser
    where emptyUser = User Map.empty Map.empty

setUser :: String -> User -> Update UserDb ()
setUser userId user = modify go
    where go (UserDb db) = UserDb $ Map.alter (\_ -> Just user) userId db

deriveSafeCopy 0 'base ''AST
deriveSafeCopy 0 'base ''Function

deriveSafeCopy 0 'base ''User
deriveSafeCopy 0 'base ''UserDb
makeAcidic ''UserDb ['getUser, 'setUser]
