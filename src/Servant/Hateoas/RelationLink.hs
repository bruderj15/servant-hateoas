{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Hateoas.RelationLink
(
  -- * Type
  RelationLink(..),
  RelationParam(..),
  mkPlaceHolder,
  appendPath,

  -- * Class
  HasRelationLink(..),
)
where

import Servant
import Servant.API.Modifiers (FoldRequired)
import GHC.TypeLits
import Data.String (fromString)
import Data.Aeson
import Data.Text (Text, intercalate)
import Data.Text.Encoding
import Data.Singletons.Bool

-- | Link data-type for hypermedia-links in HATEOAS with potentially templated URIs.
data RelationLink = RelationLink
  { _path        :: Text
  , _params      :: [RelationParam]
  , _templated   :: Bool
  , _method      :: Text
  } deriving (Show, Eq)

-- | Parameter data-type for hypermedia-links in HATEOAS.
data RelationParam = RelationParam
  { _name        :: Text
  , _required    :: Bool
  } deriving (Show, Eq)

-- | Create a placeholder for a URI template parameter.
mkPlaceHolder :: Text -> Text
mkPlaceHolder s = "{" <> s <> "}"

-- | Append a path to a URI.
appendPath :: Text -> Text -> Text
appendPath l "" = l
appendPath l r = l <> "/" <> r

instance ToJSON RelationLink where
  toJSON (RelationLink path params templated _) = String $
    if templated
    then path <> "{?" <> intercalate "," (_name <$> params) <> "}"
    else path

-- | Class for creating a 'RelationLink' to an API.
class HasRelationLink endpoint where
  toRelationLink :: Proxy endpoint -> RelationLink

instance HasRelationLink b => HasRelationLink (EmptyAPI :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance (KnownSymbol sym, HasRelationLink b) => HasRelationLink ((sym :: Symbol) :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _path = prefix `appendPath` _path rl }
    where
      prefix = fromString $ symbolVal (Proxy @sym)

instance (KnownSymbol sym, HasRelationLink b) => HasRelationLink (Capture' mods sym a :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _path = prefix `appendPath` _path rl }
    where
      prefix = mkPlaceHolder $ fromString $ symbolVal (Proxy @sym)

instance (KnownSymbol sym, HasRelationLink b) => HasRelationLink (CaptureAll sym a :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _path = prefix `appendPath` _path rl }
    where
      prefix = mkPlaceHolder $ fromString $ symbolVal (Proxy @sym)

instance HasRelationLink b => HasRelationLink (Header' mods sym a :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (HttpVersion :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance (HasRelationLink b, KnownSymbol sym, SBoolI (FoldRequired mods)) => HasRelationLink (QueryParam' mods sym a :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _params = param : _params rl, _templated = True }
    where
      param = RelationParam
        { _name = fromString $ symbolVal (Proxy @sym)
        , _required = fromSBool $ sbool @(FoldRequired mods)
        }

instance (HasRelationLink b, KnownSymbol sym) => HasRelationLink (QueryParams sym a :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _params = param : _params rl, _templated = True }
    where
      param = RelationParam
        { _name = fromString $ symbolVal (Proxy @sym)
        , _required = False
        }

instance HasRelationLink b => HasRelationLink (QueryString :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance (HasRelationLink b, KnownSymbol sym) => HasRelationLink (DeepQuery sym a :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _params = param : _params rl, _templated = True }
    where
      param = RelationParam
        { _name = fromString $ symbolVal (Proxy @sym)
        , _required = False
        }

instance HasRelationLink b => HasRelationLink (Fragment a :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (ReqBody' mods cts a :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (RemoteHost :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (IsSecure :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (Vault :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (WithNamedContext name subs sub :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (WithResource res :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance ReflectMethod m => HasRelationLink (Verb m s cts a) where
  toRelationLink _ = RelationLink
    { _path = ""
    , _params = []
    , _templated = False
    , _method = decodeUtf8 $ reflectMethod (Proxy @m)
    }

instance ReflectMethod m => HasRelationLink (NoContentVerb m) where
  toRelationLink _ = RelationLink
    { _path = ""
    , _params = []
    , _templated = False
    , _method = decodeUtf8 $ reflectMethod (Proxy @m)
    }

instance ReflectMethod m => HasRelationLink (UVerb m cts as) where
  toRelationLink _ = RelationLink
    { _path = ""
    , _params = []
    , _templated = False
    , _method = decodeUtf8 $ reflectMethod (Proxy @m)
    }

instance ReflectMethod m => HasRelationLink (Stream m s f ct a) where
  toRelationLink _ = RelationLink
    { _path = ""
    , _params = []
    , _templated = False
    , _method = decodeUtf8 $ reflectMethod (Proxy @m)
    }

instance HasRelationLink b => HasRelationLink (BasicAuth realm userData :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (Description sym :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (Summary sym :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)
