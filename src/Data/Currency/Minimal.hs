{-# LANGUAGE UndecidableInstances, DeriveAnyClass #-}
module Data.Currency.Minimal where

import Control.DeepSeq
import Data.Aeson
import Data.Data
import Data.Fixed
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Store
import Data.String(IsString(..))
import Data.Text.Conversions(ToText(..))
import Data.Text (Text)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Text as Text

class (Eq curr, Ord curr) => IsCurrency curr a | a -> curr where
  type CurrencyValueType curr a :: *
  getSymbol :: a -> curr
  applyRate :: curr -> Rational -> a -> a
  makeValue :: curr -> CurrencyValueType curr a -> a

class IsCurrency curr value => Exchange curr exch value | value -> curr where
  convertMaybe :: exch -> curr -> value -> Maybe value

convertJust :: Exchange curr exch value  => exch -> curr -> value -> value
convertJust e t v = fromJust $ convertMaybe e t v

newtype CurrencyCode = CurrencyCode Text
                       deriving (Eq,Ord,Data,Generic,NFData)

instance ToJSON CurrencyCode
instance FromJSON CurrencyCode
instance Store CurrencyCode

instance Show CurrencyCode where
  show (CurrencyCode x) = show x

instance ToText CurrencyCode where
  toText (CurrencyCode t) = t

instance IsString CurrencyCode where
  fromString s = CurrencyCode (Text.toUpper (Text.pack (take 3  s)))

newtype CurrencyValue a = CurrencyValue (CurrencyCode, a)
                          deriving (Eq,Ord,Show,Data,Generic,NFData)

instance FromJSON a => FromJSON (CurrencyValue a)
instance ToJSON a   => ToJSON (CurrencyValue a)

instance Store a => Store (CurrencyValue a)

instance (Real a, Fractional a) => IsCurrency CurrencyCode (CurrencyValue a) where
  type CurrencyValueType CurrencyCode (CurrencyValue a) = a
  makeValue c x = CurrencyValue (c,x)
  getSymbol (CurrencyValue (c, x)) = c
  applyRate to r (CurrencyValue (_,x)) = CurrencyValue (to, fromRational (r * toRational x))

makeCurrencyValue :: (Real a, Fractional a, IsString s) => s -> a -> CurrencyValue a
makeCurrencyValue code value = CurrencyValue (undefined, value)

newtype SimpleExch curr = SimpleExch (Map (curr,curr) Rational)

instance IsCurrency curr a => Exchange curr (SimpleExch curr) a where

  convertMaybe _ t val | t == getSymbol val = pure val
  convertMaybe (SimpleExch m) t val = app <$> Map.lookup (getSymbol val,t) m
    where app r = applyRate t r val

fromList :: (Ord curr, Eq curr) => [((curr,curr),Rational)] -> SimpleExch curr
fromList vals = SimpleExch m
  where s1 = vals
        s2 = [ ((c2,c1),1/r) | ((c1,c2),r) <- vals ]
        m  = Map.fromListWith (\a b -> a) (s1<>s2)

instance IsCurrency Text (Text, Fixed E2) where
  type CurrencyValueType Text (Text, Fixed E2) = Fixed E2
  applyRate curr r (_,v) = (curr, fromRational (r * toRational v))
  getSymbol = fst
  makeValue c b = (c,b)

