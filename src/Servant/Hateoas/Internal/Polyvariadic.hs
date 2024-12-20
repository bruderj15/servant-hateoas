{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Internal.Polyvariadic where

import Data.Kind

type family IsFun f where
  IsFun (_ -> _) = 'True
  IsFun _        = 'False

class (b ~ IsFun f) => PolyvariadicComp f b where
  type Return  f   b :: Type
  type Replace f r b :: Type
  pcomp :: (Return f b -> r) -> f -> Replace f r b

instance (False ~ IsFun a) => PolyvariadicComp a 'False where
  type Return  a   'False = a
  type Replace a r 'False = r
  pcomp f a = f a

instance (PolyvariadicComp b (IsFun b)) => PolyvariadicComp (a -> b) 'True where
  type Return  (a -> b)   'True =      Return  b   (IsFun b)
  type Replace (a -> b) r 'True = a -> Replace b r (IsFun b)
  pcomp final f = \x -> final `pcomp` f x

(...) :: (PolyvariadicComp f b, IsFun f ~ b) => (Return f b -> r) -> f -> Replace f r b
(...) = pcomp
infixr 9 ...

class (b ~ IsFun f, b ~ IsFun g) => PolyvariadicComp2 f g b where
  type Return2  f g   b :: Type
  type Replace2 f g r b :: Type
  pcomp2 :: (Return2 f g b -> r) -> f -> g -> Replace2 f g r b

instance (False ~ IsFun a, IsFun b ~ False) => PolyvariadicComp2 a b 'False where
  type Return2  a b   'False = (a, b)
  type Replace2 a b r 'False = r
  pcomp2 f a b = f (a, b)

instance (IsFun b ~ IsFun c, PolyvariadicComp2 b c (IsFun b)) => PolyvariadicComp2 (a -> b) (a -> c) 'True where
  type Return2  (a -> b) (a -> c)   'True =      Return2  b c   (IsFun b)
  type Replace2 (a -> b) (a -> c) r 'True = a -> Replace2 b c r (IsFun b)
  pcomp2 final f g = \x -> pcomp2 final (f x) (g x)
