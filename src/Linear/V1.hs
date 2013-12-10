{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2012-2013 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- 1-D Vectors
----------------------------------------------------------------------------
module Linear.V1
  ( V1(..)
  , R1(..)
  ) where

import Control.Applicative
import Control.Monad (liftM)
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Functor.Identity (Identity(..))
import Data.Traversable
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Functor.Bind
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Foreign.Storable (Storable)
import GHC.Arr (Ix(..))
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
import Linear.Core
import Linear.Metric
import Linear.Epsilon
import Linear.Vector
import Prelude hiding (sum)

-- $setup
-- >>> import Control.Lens

-- | A 1-dimensional vector
--
-- >>> pure 1 :: V1 Int
-- V1 1
--
-- >>> V1 2 + V1 3
-- V1 5
--
-- >>> V1 2 * V1 3
-- V1 6
--
-- >>> sum (V1 2)
-- 2

--data V2 a = V2 !a !a deriving (Eq,Ord,Show,Read,Data,Typeable)
newtype V1 a = V1 a
  deriving (Eq,Ord,Show,Read,Data,Typeable,
            Functor,Foldable,Traversable,
            Epsilon,Storable
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
           ,Generic
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
           ,Generic1
#endif
           )

instance Foldable1 V1 where
  foldMap1 f (V1 a) = f a
  {-# INLINE foldMap1 #-}

instance Traversable1 V1 where
  traverse1 f (V1 a) = V1 <$> f a
  {-# INLINE traverse1 #-}

instance Apply V1 where
  V1 f <.> V1 x = V1 (f x)
  {-@ INLINE (<.>) #-}

instance Applicative V1 where
  pure = V1
  {-# INLINE pure #-}
  V1 f <*> V1 x = V1 (f x)
  {-@ INLINE (<*>) #-}

instance Additive V1 where
  zero = pure 0
  {-# INLINE zero #-}
  liftU2 = liftA2
  {-# INLINE liftU2 #-}
  liftI2 = liftA2
  {-# INLINE liftI2 #-}

instance Bind V1 where
  V1 a >>- f = f a
  {-# INLINE (>>-) #-}

instance Monad V1 where
  return = V1
  {-# INLINE return #-}
  V1 a >>= f = f a
  {-# INLINE (>>=) #-}

instance Num a => Num (V1 a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (V1 a) where
  recip = fmap recip
  {-# INLINE recip #-}
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}

instance Metric V1 where
  dot (V1 a) (V1 b) = a * b
  {-# INLINE dot #-}

-- | A space that has at least 1 basis vector '_x'.
class R1 t where
  -- |
  -- >>> V1 2 ^._x
  -- 2
  --
  -- >>> V1 2 & _x .~ 3
  -- V1 3
  --
  -- @
  -- '_x' :: Lens' (t a) a
  -- @
  _x :: Functor f => (a -> f a) -> t a -> f (t a)

instance R1 V1 where
  _x f (V1 a) = V1 <$> f a
  {-# INLINE _x #-}

instance R1 Identity where
  _x f (Identity a) = Identity <$> f a
  {-# INLINE _x #-}

instance Core V1 where
  core f = V1 (f _x)
  {-# INLINE core #-}

instance Distributive V1 where
  distribute f = V1 (fmap (\(V1 x) -> x) f)
  {-# INLINE distribute #-}

instance Ix a => Ix (V1 a) where
  {-# SPECIALISE instance Ix (V1 Int) #-}

  range (V1 l1, V1 u1) =
    [ V1 i1 | i1 <- range (l1,u1) ]
  {-# INLINE range #-}

  unsafeIndex (V1 l1,V1 u1) (V1 i1) = unsafeIndex (l1,u1) i1
  {-# INLINE unsafeIndex #-}

  inRange (V1 l1,V1 u1) (V1 i1) = inRange (l1,u1) i1
  {-# INLINE inRange #-}

newtype instance VU.MVector s (V1 a) = MV_V1 (VU.MVector s a)
newtype instance VU.Vector    (V1 a) = V_V1  (VU.Vector a)

instance VGM.MVector VU.MVector a => VGM.MVector VU.MVector (V1 a) where
  basicLength (MV_V1 a) = VGM.basicLength a
  basicUnsafeSlice i j (MV_V1 v) = MV_V1 $ VGM.basicUnsafeSlice i j v
  basicOverlaps (MV_V1 v1) (MV_V1 v2) = VGM.basicOverlaps v1 v2
  basicUnsafeNew i = MV_V1 `liftM` VGM.basicUnsafeNew i
  basicUnsafeRead (MV_V1 v1) i = V1 `liftM` VGM.basicUnsafeRead v1 i
  basicUnsafeWrite (MV_V1 v1) i (V1 a) = VGM.basicUnsafeWrite v1 i a

instance VG.Vector VU.Vector a => VG.Vector VU.Vector (V1 a) where
  basicUnsafeFreeze (MV_V1 v) = V_V1 `liftM` VG.basicUnsafeFreeze v
  basicUnsafeThaw (V_V1 v) = MV_V1 `liftM` VG.basicUnsafeThaw v
  basicLength (V_V1 v) = VG.basicLength v
  basicUnsafeSlice i j (V_V1 v) = V_V1 $ VG.basicUnsafeSlice i j v
  basicUnsafeIndexM (V_V1 v) i = V1 `liftM` VG.basicUnsafeIndexM v i
  
instance VU.Unbox a => VU.Unbox (V1 a)