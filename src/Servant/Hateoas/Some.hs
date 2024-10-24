module Servant.Hateoas.Some where

import Data.Aeson

data SomeToJSON f where
  SomeToJSON :: ToJSON a => a -> SomeToJSON f

instance ToJSON (SomeToJSON f) where
  toJSON (SomeToJSON x) = toJSON x
