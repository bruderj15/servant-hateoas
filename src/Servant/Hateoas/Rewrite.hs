{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Rewrite where

import Servant
import Servant.Hateoas.Resource
import Data.Kind

-- Make api a tree with shared prefixes - making every choice unambiguous
type family Normalize api where
  Normalize ((prefix :> a) :<|> (prefix :> b)) = Normalize (prefix :> (Normalize a :<|> Normalize b))
  Normalize (a :<|> b)                         = Normalize a :<|> Normalize b
  Normalize ((prefix :> a) :>   (prefix :> b)) = Normalize (prefix :> (Normalize a :>   Normalize b))
  Normalize (a :> b)                           = a :> Normalize b
  Normalize a                                  = a

-- For now just wrap response type with the content-types resource.
-- May consider special cases where @a@ in @(Verb _ _ _ a)@ is already a resource.
-- Force overwrite?
-- Or simply with the content types resource again?
type Resourcify :: Type -> Type -> Type
type family Resourcify api ct where
  Resourcify (a :<|> b) ct           = Resourcify a ct :<|> Resourcify b ct
  Resourcify (a :> b) ct             = a :> Resourcify b ct
  Resourcify (Verb m s _ a) ct       = Verb m s '[ct] (MkResource ct a)
  Resourcify a _                     = a

-- Given a @ServerT api m@ and some @ct@ we want a @ServerT (Resourcify api ct) m@.
-- Current solution is a little bit hacky, but it works as long as @m@ is not nested.
type family ResourcifyServer server ct m where
  ResourcifyServer (a :<|> b) ct m = ResourcifyServer a ct m :<|> ResourcifyServer b ct m
  ResourcifyServer (a -> b)   ct m = a -> ResourcifyServer b ct m
  ResourcifyServer (m a)      ct m = m (MkResource ct a)
  ResourcifyServer (f a)      ct m = f (ResourcifyServer a ct m)

data HBranch = HBranch
  { nodeApi      :: Type      -- relative path from host: e.g. /api/users
  , childrenApis :: [Type]    -- immediate relative children paths from host: e.g. [/api/users/1]
  }

type (++) xs ys = AppendList xs ys

-- Wrapping api in: Bottom :> api :> Top, so api has kind k and not Type.
-- This is crucial so we can match paths (:: Symbol) and potential other-kinded combinators
data Bottom
data Top

-- Creates all intermediate layers of the api and their immediate children
type HLayers :: p -> q -> [HBranch]
type family HLayers api stand where
  HLayers (a :<|> b)  Bottom                   = HLayers a Bottom ++ HLayers b Bottom
  HLayers (a :<|> b) (Bottom :> prefix :> Top) = HLayers a (Bottom :> prefix :> Top) ++ HLayers b (Bottom :> prefix :> Top)
  HLayers (a :> b)    Bottom                   = '[ 'HBranch  Bottom                   (FirstPath a Bottom) ] ++ HLayers b (Bottom           :> a :> Top)
  HLayers (a :> b)   (Bottom :> prefix :> Top) = '[ 'HBranch (Bottom :> prefix :> Top) (FirstPath a prefix) ] ++ HLayers b (Bottom :> prefix :> a :> Top)
  HLayers _ _                                  = '[]

-- Interpreting api as a tree returning the first layers of the tree
type FirstPath :: p -> q -> [Type]
type family FirstPath api prefix where
  FirstPath (a :<|> b) prefix = FirstPath a prefix ++ FirstPath b prefix
  FirstPath (a :> _)   Bottom = '[Bottom           :> a :> Top]
  FirstPath (a :> _)   prefix = '[Bottom :> prefix :> a :> Top]
  FirstPath a          Bottom = '[Bottom           :> a :> Top]
  FirstPath a          prefix = '[Bottom :> prefix :> a :> Top]
