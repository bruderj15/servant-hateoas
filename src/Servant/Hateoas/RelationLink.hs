{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Servant.Hateoas.RelationLink
(
  -- * RelationLink
  -- ** Type
  RelationLink(..),
  RelationParam(..),

  -- *** Creation
  fromURI,

  -- *** Operations
  mkPlaceHolder,
  appendPath,

  -- ** Class
  HasRelationLink(..),

  -- * Utility
  -- ** ReflectStdMethod
  reflectStdMethod,
)
where

import Prelude hiding (drop, dropWhile, break)
import Servant
import Servant.API.ContentTypes (AllMime(..))
import Servant.API.Modifiers (FoldRequired)
import Network.URI (unEscapeString)
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types (parseMethod, Method)
import Data.String (fromString)
import Data.Aeson
import Data.Text (Text, intercalate, dropWhile, split, break, drop)
import Data.Singletons.Bool
import GHC.TypeLits

-- | Link data-type for hypermedia-links in HATEOAS with potentially templated URIs.
data RelationLink = RelationLink
  { _path         :: Text
  , _params       :: [RelationParam]
  , _fragment     :: Maybe Text
  , _templated    :: Bool
  , _method       :: StdMethod
  , _contentTypes :: [MediaType]
  , _summary      :: Maybe Text
  , _description  :: Maybe Text
  } deriving (Show, Eq)

-- | Parameter data-type for hypermedia-links in HATEOAS.
data RelationParam = RelationParam
  { _name        :: Text
  , _required    :: Bool
  , _value       :: Maybe Text
  } deriving (Show, Eq)

-- | Create a placeholder for a URI template parameter.
mkPlaceHolder :: Text -> Text
mkPlaceHolder s = "{" <> s <> "}"

-- | Append a path to a URI.
appendPath :: Text -> Text -> Text
appendPath l "" = l
appendPath l r = l <> "/" <> r

-- | Creates a 'RelationLink' from an 'URI'.
fromURI :: [MediaType] -> StdMethod -> URI -> RelationLink
fromURI cts m (URI _ _ path query frag) = RelationLink
  { _path = fromString path
  , _params = params
  , _fragment = if frag == "" then Nothing else Just $ fromString frag
  , _templated = False
  , _method = m
  , _contentTypes = cts
  , _summary = Nothing
  , _description = Nothing
  }
  where
    params = filter ((/= "") . _name)
      $ fmap (\kv -> let (k, drop 1 -> v) = break (== '=') kv in RelationParam k False $ if v == "" then Nothing else Just v)
      $ split (== '&')
      $ dropWhile (== '?')
      $ fromString
      $ unEscapeString query

-- | Like 'reflectMethod' but returns a 'StdMethod'.
reflectStdMethod :: ReflectMethod method => Proxy method -> StdMethod
reflectStdMethod = unsafeMethodToStdMethod . reflectMethod

unsafeMethodToStdMethod :: Method -> StdMethod
unsafeMethodToStdMethod (parseMethod -> Right m) = m
unsafeMethodToStdMethod (parseMethod -> Left  m) = error $ "Cannot convert " <> show m <> " to StdMethod"

instance ToJSON RelationLink where
  toJSON (RelationLink path params frag templated _ _ _ _) = String $
    if not (null params) && templated
    then path <> "{?" <> intercalate "," (_name <$> params) <> "}" <> maybe "" (\f -> "#" <> f) frag
    else path <> maybe "" (\f -> "#" <> f) frag

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
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _path = prefix `appendPath` _path rl, _templated = True }
    where
      prefix = mkPlaceHolder $ fromString $ symbolVal (Proxy @sym)

instance (KnownSymbol sym, HasRelationLink b) => HasRelationLink (CaptureAll sym a :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _path = prefix `appendPath` _path rl, _templated = True }
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
        , _value = Nothing
        }

instance (HasRelationLink b, KnownSymbol sym) => HasRelationLink (QueryParams sym a :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _params = param : _params rl, _templated = True }
    where
      param = RelationParam
        { _name = fromString $ symbolVal (Proxy @sym)
        , _required = False
        , _value = Nothing
        }

instance (HasRelationLink b, KnownSymbol sym) => HasRelationLink (QueryFlag sym :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _params = param : _params rl, _templated = True }
    where
      param = RelationParam
        { _name = fromString $ symbolVal (Proxy @sym)
        , _required = False
        , _value = Nothing
        }

instance HasRelationLink b => HasRelationLink (QueryString :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance (HasRelationLink b, KnownSymbol sym) => HasRelationLink (DeepQuery sym a :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _params = param : _params rl, _templated = True }
    where
      param = RelationParam
        { _name = fromString $ symbolVal (Proxy @sym)
        , _required = False
        , _value = Nothing
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

instance (ReflectMethod m, AllMime cts) => HasRelationLink (Verb m s cts a) where
  toRelationLink _ = RelationLink
    { _path = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = allMime (Proxy @cts)
    }

instance ReflectMethod m => HasRelationLink (NoContentVerb m) where
  toRelationLink _ = RelationLink
    { _path = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = mempty
    }

instance (ReflectMethod m, AllMime cts) => HasRelationLink (UVerb m cts as) where
  toRelationLink _ = RelationLink
    { _path = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = allMime (Proxy @cts)
    }

instance (ReflectMethod m, Accept ct) => HasRelationLink (Stream m s f ct a) where
  toRelationLink _ = RelationLink
    { _path = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = pure $ contentType (Proxy @ct)
    }

instance HasRelationLink b => HasRelationLink (BasicAuth realm userData :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance (KnownSymbol sym, HasRelationLink b) => HasRelationLink (Description sym :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _description = Just descr }
    where
      descr = fromString $ symbolVal (Proxy @sym)

instance (KnownSymbol sym, HasRelationLink b) => HasRelationLink (Summary sym :> b) where
  toRelationLink _ = let rl = toRelationLink (Proxy @b) in rl { _summary = Just summary }
    where
      summary = fromString $ symbolVal (Proxy @sym)
