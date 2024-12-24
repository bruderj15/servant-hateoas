{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Internal.Polyvariadic
(
  -- * IsFunction
  IsFun,

  -- * Simple Polyvariadic composition
  PolyvariadicComp(..),
  (...),

  -- * Polyvariadic composition with two functions
  PolyvariadicComp2(..),
)
where

import Data.Kind

-- | Type-level function to determine if a 'Type' is a function.
type family IsFun f where
  IsFun (_ -> _) = 'True
  IsFun _        = 'False

-- | Class for polyvariadic composition.
--
-- This is copied from the package @erisco/control-dotdotdot@.
class (b ~ IsFun f) => PolyvariadicComp f b where
  type Return  f   b :: Type
  type Replace f r b :: Type
  -- | @pcomp f g@ has @g@ consume all arguments and then has @f@ consume the result of @g@.
  pcomp :: (Return f b -> r) -> f -> Replace f r b

instance (False ~ IsFun a) => PolyvariadicComp a 'False where
  type Return  a   'False = a
  type Replace a r 'False = r
  pcomp f a = f a

instance (PolyvariadicComp b (IsFun b)) => PolyvariadicComp (a -> b) 'True where
  type Return  (a -> b)   'True =      Return  b   (IsFun b)
  type Replace (a -> b) r 'True = a -> Replace b r (IsFun b)
  pcomp final f = \x -> final `pcomp` f x

-- | Infix for 'pcomp'.
(...) :: (PolyvariadicComp f b, IsFun f ~ b) => (Return f b -> r) -> f -> Replace f r b
(...) = pcomp
infixr 9 ...

-- | Like 'PolyvariadicComp' but allows to consume all arguments twice,
-- by two functions with the exact same arguments but potentially different return types.
class (b ~ IsFun f, b ~ IsFun g) => PolyvariadicComp2 f g b where
  type Return2  f g   b :: Type
  type Replace2 f g r b :: Type
  -- | @pcomp2 f g h@ has each @g@ and @h@ consume all arguments and then has @f@ consume the result of @g@ and @h@.
  --
  -- This is highly similar to '(&&&)' from 'Control.Arrow' but for polyvariadic composition.
  pcomp2 :: (Return2 f g b -> r) -> f -> g -> Replace2 f g r b

instance (False ~ IsFun a, IsFun b ~ False) => PolyvariadicComp2 a b 'False where
  type Return2  a b   'False = (a, b)
  type Replace2 a b r 'False = r
  pcomp2 f a b = f (a, b)

instance (IsFun b ~ IsFun c, PolyvariadicComp2 b c (IsFun b)) => PolyvariadicComp2 (a -> b) (a -> c) 'True where
  type Return2  (a -> b) (a -> c)   'True =      Return2  b c   (IsFun b)
  type Replace2 (a -> b) (a -> c) r 'True = a -> Replace2 b c r (IsFun b)
  pcomp2 final f g = \x -> pcomp2 final (f x) (g x)
