module Servant.Hateoas.Resource
( Resource(..)
, HasResource(..)
, addRel, addRels
) where

import Servant.Links
import Data.Proxy
import GHC.Generics

-- | RESTy Resource representation of a datatype.
data Resource a = Resource
  { resource :: a
  , links    :: [(String, Link)]
  } deriving (Show, Generic)

-- | Add a relation to another resource.
addRel :: (String, Link) -> Resource a -> Resource a
addRel l (Resource x ls) = Resource x (l:ls)
{-# INLINE addRel #-}

-- | Add multiple relations to other resources.
addRels :: [(String, Link)] -> Resource a -> Resource a
addRels ls' (Resource x ls) = Resource x $ ls' ++ ls
{-# INLINE addRels #-}

-- | Datatypes that can be represented as 'Resource's.
class HasResource api a where
  -- | Creates the resource representation of a value.
  toResource :: Proxy api -> a -> Resource a
