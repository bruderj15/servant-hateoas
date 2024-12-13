{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.Rewrite where

import Servant

-- Make api a tree with shared prefixes - making every choice unambiguous
type family Normalize api where
  Normalize ((prefix :> a) :<|> (prefix :> b)) = Normalize (prefix :> (Normalize a :<|> Normalize b))
  Normalize (a :<|> b)                         = Normalize a :<|> Normalize b
  Normalize ((prefix :> a) :>   (prefix :> b)) = Normalize (prefix :> (Normalize a :>   Normalize b))
  Normalize (a :> b)                           = a :> Normalize b
  Normalize a                                  = a
