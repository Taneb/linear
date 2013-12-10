{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}
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
-- 0-D Vectors
----------------------------------------------------------------------------
module Linear.V0
  ( V0(..)
  ) where

import Control.Applicative
import Data.Data
import Data.Distributive
import Data.Foldable
import Data.Ix
import Data.Traversable
import Data.Semigroup
import Data.Functor.Bind
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif
import Foreign.Storable (Storable(..))
import Linear.Core
import Linear.Metric
import Linear.Epsilon
import Linear.Vector
import Prelude hiding (sum)

-- $setup
-- >>> import Control.Lens

-- | A 0-dimensional vector
--
-- >>> pure 1 :: V0 Int
-- V0
--
-- >>> V0 + V0
-- V0
--
data V0 a = V0 deriving (Eq,Ord,Show,Read,Ix,Enum,Data,Typeable
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
                        ,Generic
#endif
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
                        ,Generic1
#endif
                        )

instance Functor V0 where
  fmap _ V0 = V0
  {-# INLINE fmap #-}
  _ <$ _ = V0
  {-# INLINE (<$) #-}

instance Foldable V0 where
  foldMap _ V0 = mempty
  {-# INLINE foldMap #-}

instance Traversable V0 where
  traverse _ V0 = pure V0
  {-# INLINE traverse #-}

instance Apply V0 where
  V0 <.> V0 = V0
  {-@ INLINE (<.>) #-}

instance Applicative V0 where
  pure _ = V0
  {-# INLINE pure #-}
  V0 <*> V0 = V0
  {-# INLINE (<*>) #-}

instance Additive V0 where
  zero = V0
  {-# INLINE zero #-}
  liftU2 _ V0 V0 = V0
  {-# INLINE liftU2 #-}
  liftI2 _ V0 V0 = V0
  {-# INLINE liftI2 #-}

instance Bind V0 where
  V0 >>- _ = V0
  {-# INLINE (>>-) #-}

instance Monad V0 where
  return _ = V0
  {-# INLINE return #-}
  V0 >>= _ = V0
  {-# INLINE (>>=) #-}

instance Num (V0 a) where
  V0 + V0 = V0
  {-# INLINE (+) #-}
  V0 - V0 = V0
  {-# INLINE (-) #-}
  V0 * V0 = V0
  {-# INLINE (*) #-}
  negate V0 = V0
  {-# INLINE negate #-}
  abs V0 = V0
  {-# INLINE abs #-}
  signum V0 = V0
  {-# INLINE signum #-}
  fromInteger _ = V0
  {-# INLINE fromInteger #-}

instance Fractional (V0 a) where
  recip _ = V0
  {-# INLINE recip #-}
  V0 / V0 = V0
  {-# INLINE (/) #-}
  fromRational _ = V0
  {-# INLINE fromRational #-}

instance Metric V0 where
  dot V0 V0 = 0
  {-# INLINE dot #-}

instance Core V0 where
  core _ = V0
  {-# INLINE core #-}

instance Distributive V0 where
  distribute _ = V0
  {-# INLINE distribute #-}

instance Epsilon a => Epsilon (V0 a) where
  nearZero _ = True
  {-# INLINE nearZero #-}

instance Storable a => Storable (V0 a) where
  sizeOf _ = 0
  {-# INLINE sizeOf #-}
  alignment _ = 1
  {-# INLINE alignment #-}
  poke _ V0 = return ()
  {-# INLINE poke #-}
  peek _ = return V0
  {-# INLINE peek #-}

newtype instance VU.MVector s (V0 a) = MV_V0 Int
newtype instance VU.Vector    (V0 a) = V_V0 Int

instance VGM.MVector VU.MVector (V0 a) where
  basicLength (MV_V0 n) = n
  basicUnsafeSlice _ m (MV_V0 _) = MV_V0 m
  basicOverlaps _ _ = False
  basicUnsafeNew i = return (MV_V0 i)
  basicUnsafeRead _ _ = return V0
  basicUnsafeWrite _ _ _ = return ()

instance VG.Vector VU.Vector (V0 a) where
  basicUnsafeFreeze (MV_V0 i) = return (V_V0 i)
  basicUnsafeThaw (V_V0 i) = return (MV_V0 i)
  basicLength (V_V0 i) = i
  basicUnsafeSlice _ m _ = V_V0 m
  basicUnsafeIndexM _ _ = return V0