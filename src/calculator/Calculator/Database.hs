module Calculator.Database (
   Calculator.Database.UserState,
   getUserData
) where

import Calculator.Database.UserState
import Calculator.Config
import Data.Acid (AcidState, openLocalStateFrom)
import Data.Acid.Advanced (query', update')

getDb :: IO (AcidState UserDb)
getDb = openLocalStateFrom (getConfigValue "db") UserState.emptyState

getUserData :: AcidState UserDb -> String -> User
getUserData acid userId = query' acid (UserState.GetUser userId)
