-- Functor instances for the following functions
-- Maybe: Type -> Type
-- Either: Type -> Type -> Type
-- Sum (+): Type (Num) -> Type -> Type
-- Product (*): Type (Num) -> Type -> Type
-- Min: Type (Ord) -> Type -> Type
-- Max: Type (Ord) -> Type -> Type
-- First: Type -> Type
-- Last: Type -> Type

import Prelude hiding (Functor (..))
-- import Data.Monoid hiding (First, Last)
import Data.Semigroup

-- Think of these as transformations of TYPES, not VALUES.
-- Functors/fmap take a functions and wrap the function with specified Types.
-- You're figuring out how to work with each Type over figuring out how to change the value
-- For a list, changing each element of a tree. For a Proxy, not doing anything. For a Maybe, only applying to Just

class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Functor (Either l) where
  fmap :: (a -> b) -> Either l a -> Either l b 
  -- The l doesn't change, and there's nothing we can do with it
  -- We only convert a's to b's because l's are untouchable
  fmap g (Left l) = Left l
  fmap g (Right a) = Right (g a)

-- data Sum a = Sum a
-- This is correct
instance Functor Sum where
  fmap f (Sum a) = Sum (f a)

instance Functor Product where
  fmap f (Product a) = Product (f a)

instance Functor Min where
  fmap f (Min a) = Min (f a)

instance Functor Max where
  fmap f (Max a) = Max (f a)

instance Functor First where
  fmap f (First a) = First (f a)

instance Functor Last where
  fmap f (Last a) = Last (f a)