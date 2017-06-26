--
-- | custom versions of modifiers with nicer Show instances
--
module Codex.QuickCheck.Modifiers where

import Data.List (nub)
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property

newtype NonNegative a
  = NonNegative {getNonNegative :: a} deriving (Eq, Ord)

newtype Positive a
  = Positive {getPositive :: a} deriving (Eq, Ord)

newtype NonZero a
  = NonZero {getNonZero :: a} deriving (Eq, Ord)

newtype OrderedList a
  = Ordered {getOrdered :: [a]} deriving (Eq, Ord)

newtype StrictOrderedList a
  = StrictOrdered {getStrictOrdered :: [a]} deriving (Eq, Ord)

newtype NonEmptyList a
  = NonEmpty {getNonEmpty :: [a]} deriving (Eq, Ord)

instance Show a => Show (NonNegative a) where
  showsPrec prec (NonNegative x) = showsPrec prec x

instance Show a => Show (Positive a) where
  showsPrec prec (Positive x) = showsPrec prec x

instance Show a => Show (NonZero a) where
  showsPrec prec (NonZero x) = showsPrec prec x

instance Show a => Show (OrderedList a) where
  showsPrec prec (Ordered xs) = showsPrec prec xs

instance Show a => Show (StrictOrderedList a) where
  showsPrec prec (StrictOrdered xs) = showsPrec prec xs
  
instance Show a => Show (NonEmptyList a) where
  showsPrec prec (NonEmpty xs) = showsPrec prec xs

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary = NonNegative <$> fmap abs arbitrary
  shrink (NonNegative x) = [ NonNegative x'
                           | x' <- shrink x,
                             x'>=0
                           ]

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> (arbitrary `suchThat` (>0))
  shrink (Positive x) = [ Positive x'
                        | x'<-shrink x,
                          x'>0
                        ]

instance (Num a, Eq a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary = NonZero <$> (arbitrary `suchThat` (/=0))
  shrink (NonZero x) = [ NonZero x'
                       | x'<-shrink x,
                         x'/=0
                       ]

instance (Arbitrary a, Ord a) => Arbitrary (OrderedList a) where
  arbitrary = Ordered <$> orderedList
  shrink (Ordered xs) = [ Ordered xs'
                        | xs'<-shrink xs,
                          ordered xs'
                        ]
    where ordered xs = and $ zipWith (<=) xs (tail xs)


instance (Arbitrary a, Ord a) => Arbitrary (StrictOrderedList a) where
  arbitrary = (StrictOrdered . nub) <$> orderedList
  shrink (StrictOrdered xs) = [StrictOrdered xs'
                              | xs'<-shrink xs,
                                ordered xs'
                              ]
    where ordered xs = and $ zipWith (<) xs (tail xs)


instance Arbitrary a => Arbitrary (NonEmptyList a) where
  arbitrary = NonEmpty <$> (arbitrary `suchThat` (not.null))
  shrink (NonEmpty xs) = [ NonEmpty xs'
                         | xs'<-shrink xs,
                           not (null xs')
                         ]





