{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UserState where

import Calculator.Data.AST (AST(..))
import Calculator.Functions (Function(..))
import Control.Applicative ((<$>))
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Acid
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Number.CReal
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as Map

data User = User { getVars :: Map String AST
                 , getFuncs :: Map String Function
                 } deriving (Show, Typeable)

data UserDb = UserDb { allUsers :: Map String User
                     } deriving (Typeable)

emptyState :: UserDb
emptyState = UserDb Map.empty

newUser :: User
newUser = User Map.empty Map.empty

getUser :: String -> Query UserDb User
getUser userId = do
    db <- ask
    let maybeUser = Map.lookup userId $ allUsers db
    return $ fromMaybe newUser maybeUser

setUser :: String -> User -> Update UserDb ()
setUser userId user = modify go
    where go (UserDb db) = UserDb $ Map.alter (\_ -> Just user) userId db

instance SafeCopy CReal where
     putCopy n = contain $ safePut $ show n
     getCopy = contain $ read <$> safeGet

deriveSafeCopy 0 'base ''AST
deriveSafeCopy 0 'base ''Function

deriveSafeCopy 0 'base ''User
deriveSafeCopy 0 'base ''UserDb
makeAcidic ''UserDb ['getUser, 'setUser]
