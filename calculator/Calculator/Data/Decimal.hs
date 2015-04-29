{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Calculator.Data.Decimal (
    Decimal(..)
) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Data
import Data.Number.CReal
import Data.Ratio ((%))
import Data.SafeCopy

newtype Decimal = Decimal CReal
                  deriving (Typeable, Enum, Eq, Floating, Fractional, Num, Ord, RealFloat, RealFrac)

instance Data Decimal where
    toConstr = mkRealConstr decimalType
    gunfold _ _ = error "Calculator.Data.Decimal.gunfold"
    dataTypeOf _ = decimalType

decimalType :: DataType
decimalType = mkFloatType "Calculator.Data.Decimal"

isInt :: Decimal -> Bool
isInt n = (floor n :: Integer) == ceiling n

instance Show Decimal where
    show (Decimal d) = show d

instance Read Decimal where
    readsPrec s = map (first Decimal) . readsPrec s

instance Real Decimal where
    toRational n = let s = until (\p -> isInt $ p * n) (10 *) 1
                   in floor (s * n) % floor s

instance SafeCopy Decimal where
     putCopy n = contain $ safePut $ show n
     getCopy = contain $ liftM read safeGet
