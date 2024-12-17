{-# LANGUAGE DerivingStrategies #-}

module Servant.Hateoas.HasTemplateLink where

import Servant
import Data.Aeson
import GHC.TypeLits

newtype TemplateLink = TemplateLink { templateLink :: URI }
  deriving newtype (Show, Eq, Ord, ToJSON)

class HasTemplateLink endpoint where
  toTemplateLink :: Proxy endpoint -> TemplateLink

instance KnownSymbol sym => HasTemplateLink (sym :: Symbol) where
  toTemplateLink sym = TemplateLink $
    URI { uriScheme    = mempty
        , uriAuthority = Nothing
        , uriPath      = symbolVal sym
        , uriQuery     = mempty
        , uriFragment  = mempty
        }

instance HasTemplateLink (Verb m s cts a) where
  toTemplateLink _ = TemplateLink $
    URI { uriScheme    = mempty
        , uriAuthority = Nothing
        , uriPath      = mempty
        , uriQuery     = mempty
        , uriFragment  = mempty
        }

instance KnownSymbol sym => HasTemplateLink (Capture sym a) where
  toTemplateLink _ = TemplateLink $
    URI { uriScheme    = mempty
        , uriAuthority = Nothing
        , uriPath      = "{" <> symbolVal (Proxy @sym) <> "}"
        , uriQuery     = mempty
        , uriFragment  = mempty
        }

instance (KnownSymbol sym, HasTemplateLink b) => HasTemplateLink ((sym :: Symbol) :> b) where
  toTemplateLink _ = let TemplateLink uri = toTemplateLink (Proxy @b)
                      in TemplateLink $ uri { uriPath = symbolVal (Proxy @sym) <> "/" <> uriPath uri }

instance (KnownSymbol sym, HasTemplateLink b) => HasTemplateLink (Capture sym a :> b) where
  toTemplateLink _ = let TemplateLink uri = toTemplateLink (Proxy @b)
                      in TemplateLink $ uri { uriPath = "{" <> symbolVal (Proxy @sym) <> "}/" <> uriPath uri }
