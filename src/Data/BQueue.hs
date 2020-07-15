{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BQueue
-- Copyright   :  (C) 2020 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <djohnson.m@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Data.BQueue
  ( -- * Type
    BQueue (..)
    -- * Methods
  , empty
  , fromList
  , toList
  , enqueue
  , dequeue
  ) where

import           Data.Data     (Data)
import qualified Data.Foldable as F
import           Data.List     (foldl')
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)
import           Prelude       hiding (seq)

-- | Bound information, specified by user
type Bound = Int

-- | Bounded Queue implementation
data BQueue a = BQueue !Bound !(Seq a)
  deriving (Show, Eq, Typeable, Generic, Data, Read, Ord)

instance Semigroup (BQueue a) where
  BQueue bound1 s1 <> BQueue bound2 s2 =
    BQueue (bound1 + bound2) (s1 <> s2)

instance Monoid (BQueue a) where
  mempty = mempty

instance Functor BQueue where
  fmap f (BQueue bound s) = BQueue bound (fmap f s)

instance Applicative BQueue where
  pure x = BQueue 1 (pure x)
  BQueue b1 f <*> BQueue b2 m =
    BQueue (b1 + b2) (f <*> m)

instance Monad BQueue where
  return = pure
  (>>=) :: BQueue a -> (a -> BQueue b) -> BQueue b
  m >>= f = foldMap f (toList m)

instance Foldable BQueue where
  foldr f x (BQueue _ s) = foldr f x s

instance Traversable BQueue where
  traverse f (BQueue bound s) = BQueue bound <$> traverse f s

-- | Construct an empty 'BQueue', /O(1)/
empty :: Bound -> BQueue a
empty bound = BQueue bound mempty

-- | Convert a 'BQueue' to a list, /O(n)/
toList :: BQueue a -> [a]
toList (BQueue _ s) = F.toList s

-- | Construct a 'BQueue' from a list, /O(n)/
fromList :: Int -> [a] -> BQueue a
fromList bound =
  foldl' (\b a -> snd (enqueue a b))
    (empty bound)

-- | Enqueue an item onto a 'BQueue', /O(1)/
-- Returns an item if the bound has been exceeded
enqueue :: a -> BQueue a -> (Maybe a, BQueue a)
enqueue x (BQueue bound xs)
  | S.length xs >= bound =
     case S.viewr xs of
       ys S.:> leftOver ->
         (Just leftOver, BQueue bound (ys S.|> x))
       S.EmptyR ->
         (Nothing, BQueue bound mempty)
  | otherwise =
      (Nothing, BQueue bound (xs S.|> x))

-- | Dequeue an item from a 'BQueue', /O(1)/
-- Returns the item along with the updated 'BQueue'
dequeue :: BQueue a -> (Maybe a, BQueue a)
dequeue (BQueue bound s)
  | S.null s = (Nothing, BQueue bound mempty)
  | otherwise =
    case S.viewl s of
      y S.:< ys -> (Just y, BQueue bound ys)
      S.EmptyL -> (Nothing, BQueue bound s)


