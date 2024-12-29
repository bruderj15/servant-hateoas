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
  fromLink,
  fromURI,

  -- *** Operations
  (<<<),
  getHref,
  getPath,
  getParams,
  prependSeg,
  prependSegs,
  addParam,
  addParams,
  mkPlaceHolder,
  appendPath,

  -- ** Class
  HasRelationLink(..),
  HasTemplatedLink(..),

  -- * Utility
  -- ** ReflectStdMethod
  reflectStdMethod,
)
where

import Prelude hiding (drop, dropWhile, break)
import Servant
import Servant.API.ContentTypes (AllMime(..))
import Servant.API.Modifiers (FoldRequired)
import Servant.Hateoas.Internal.Polyvariadic
import Network.URI (unEscapeString, pathSegments)
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types (parseMethod, Method)
import Data.Foldable (foldl')
import Data.String (fromString)
import Data.Aeson
import Data.Text (Text, intercalate, dropWhile, split, break, drop, isPrefixOf, isSuffixOf)
import Data.Singletons.Bool
import Control.Applicative ((<|>))
import GHC.TypeLits

-- | Link data-type for hypermedia-links in HATEOAS with potentially templated URIs.
data RelationLink = RelationLink
  { _segs         :: [Text]
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

(<<<) :: RelationLink -> RelationLink -> RelationLink
l1 <<< l2 =
  l1 { _segs        = _segs        l1  <> _segs        l2
     , _params      = _params      l1  <> _params      l2
     , _fragment    = _fragment    l1 <|> _fragment    l2
     , _templated   = _templated   l1  || _templated   l2
     , _summary     = _summary     l2 <|> _summary     l1
     , _description = _description l2 <|> _description l1
     }

getHref :: RelationLink -> Text
getHref l = getPath l <> getParams l <> maybe "" (\f -> "#" <> f) (_fragment l)

getPath :: RelationLink -> Text
getPath = ("/" <>) .  intercalate "/" . _segs

getParams :: RelationLink -> Text
getParams link =
     (if filledParams == [] then "" else "?" <> intercalate "&" (fmap (\(k,v) -> k <> "=" <> v) filledParams))
  <> (if templatedParams == [] then "" else "{?" <> intercalate "," templatedParams <> "}")
  where
    (filledParams, templatedParams) =
      foldl'
        (\(fs, ts) l -> case _value l of Nothing -> (fs, _name l : ts) ; Just v -> ((_name l, v) : fs, ts))
        ([], []) $
        _params link

prependSeg :: Text -> RelationLink -> RelationLink
prependSeg seg l
  | "{" `isPrefixOf` seg && "}" `isSuffixOf` seg = l { _segs = seg : _segs l, _templated = True }
  | otherwise = l { _segs = seg : _segs l }

prependSegs :: [Text] -> RelationLink -> RelationLink
prependSegs segs l
  | any (\seg -> "{" `isPrefixOf` seg && "}" `isSuffixOf` seg) segs = l { _segs = segs <> _segs l, _templated = True }
  | otherwise                                                       = l { _segs = segs <> _segs l }

addParam :: RelationParam -> RelationLink -> RelationLink
addParam p l = l { _params = p : _params l }

addParams :: [RelationParam] -> RelationLink -> RelationLink
addParams ps l = l { _params = ps <> _params l }

-- | Create a placeholder for a URI template parameter.
mkPlaceHolder :: Text -> Text
mkPlaceHolder s = "{" <> s <> "}"

-- | Append a path to a URI.
appendPath :: Text -> Text -> Text
appendPath l "" = l
appendPath l r = l <> "/" <> r

fromLink :: [MediaType] -> StdMethod -> Link -> RelationLink
fromLink cts m = fromURI cts m . linkURI

-- | Creates a 'RelationLink' from an 'URI'.
fromURI :: [MediaType] -> StdMethod -> URI -> RelationLink
fromURI cts m uri@(URI _ _ _ query frag) = RelationLink
  { _segs = fromString <$> pathSegments uri
  , _params = params
  , _fragment = if frag == "" then Nothing else Just (fromString frag)
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
  toJSON = String . getHref

-- | Class for creating a 'RelationLink' to an API.
class HasTemplatedLink endpoint where
  toTemplatedLink :: Proxy endpoint -> RelationLink

instance HasTemplatedLink b => HasTemplatedLink (EmptyAPI :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance (KnownSymbol sym, HasTemplatedLink b) => HasTemplatedLink ((sym :: Symbol) :> b) where
  toTemplatedLink _ = prependSeg prefix $ toTemplatedLink (Proxy @b)
    where
      prefix = fromString $ symbolVal (Proxy @sym)

instance (KnownSymbol sym, HasTemplatedLink b) => HasTemplatedLink (Capture' mods sym a :> b) where
  toTemplatedLink _ = prependSeg prefix $ toTemplatedLink (Proxy @b)
    where
      prefix = mkPlaceHolder $ fromString $ symbolVal (Proxy @sym)

instance (KnownSymbol sym, HasTemplatedLink b) => HasTemplatedLink (CaptureAll sym a :> b) where
  toTemplatedLink _ = prependSeg prefix $ toTemplatedLink (Proxy @b)
    where
      prefix = mkPlaceHolder $ fromString $ symbolVal (Proxy @sym)

instance HasTemplatedLink b => HasTemplatedLink (Header' mods sym a :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance HasTemplatedLink b => HasTemplatedLink (HttpVersion :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance (HasTemplatedLink b, KnownSymbol sym, SBoolI (FoldRequired mods)) => HasTemplatedLink (QueryParam' mods sym a :> b) where
  toTemplatedLink _ = let rl = toTemplatedLink (Proxy @b) in rl { _params = param : _params rl, _templated = True }
    where
      param = RelationParam
        { _name = fromString $ symbolVal (Proxy @sym)
        , _required = fromSBool $ sbool @(FoldRequired mods)
        , _value = Nothing
        }

instance (HasTemplatedLink b, KnownSymbol sym) => HasTemplatedLink (QueryParams sym a :> b) where
  toTemplatedLink _ = let rl = toTemplatedLink (Proxy @b) in rl { _params = param : _params rl, _templated = True }
    where
      param = RelationParam
        { _name = fromString $ symbolVal (Proxy @sym)
        , _required = False
        , _value = Nothing
        }

instance (HasTemplatedLink b, KnownSymbol sym) => HasTemplatedLink (QueryFlag sym :> b) where
  toTemplatedLink _ = let rl = toTemplatedLink (Proxy @b) in rl { _params = param : _params rl, _templated = True }
    where
      param = RelationParam
        { _name = fromString $ symbolVal (Proxy @sym)
        , _required = False
        , _value = Nothing
        }

instance HasTemplatedLink b => HasTemplatedLink (QueryString :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance (HasTemplatedLink b, KnownSymbol sym) => HasTemplatedLink (DeepQuery sym a :> b) where
  toTemplatedLink _ = let rl = toTemplatedLink (Proxy @b) in rl { _params = param : _params rl, _templated = True }
    where
      param = RelationParam
        { _name = fromString $ symbolVal (Proxy @sym)
        , _required = False
        , _value = Nothing
        }

instance HasTemplatedLink b => HasTemplatedLink (Fragment a :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance HasTemplatedLink b => HasTemplatedLink (ReqBody' mods cts a :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance HasTemplatedLink b => HasTemplatedLink (RemoteHost :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance HasTemplatedLink b => HasTemplatedLink (IsSecure :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance HasTemplatedLink b => HasTemplatedLink (Vault :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance HasTemplatedLink b => HasTemplatedLink (WithNamedContext name subs sub :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance HasTemplatedLink b => HasTemplatedLink (WithResource res :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance (ReflectMethod m, AllMime cts) => HasTemplatedLink (Verb m s cts a) where
  toTemplatedLink _ = RelationLink
    { _segs = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = allMime (Proxy @cts)
    }

instance ReflectMethod m => HasTemplatedLink (NoContentVerb m) where
  toTemplatedLink _ = RelationLink
    { _segs = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = mempty
    }

instance (ReflectMethod m, AllMime cts) => HasTemplatedLink (UVerb m cts as) where
  toTemplatedLink _ = RelationLink
    { _segs = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = allMime (Proxy @cts)
    }

instance (ReflectMethod m, Accept ct) => HasTemplatedLink (Stream m s f ct a) where
  toTemplatedLink _ = RelationLink
    { _segs = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = pure $ contentType (Proxy @ct)
    }

instance HasTemplatedLink b => HasTemplatedLink (BasicAuth realm userData :> b) where
  toTemplatedLink _ = toTemplatedLink (Proxy @b)

instance (KnownSymbol sym, HasTemplatedLink b) => HasTemplatedLink (Description sym :> b) where
  toTemplatedLink _ = let rl = toTemplatedLink (Proxy @b) in rl { _description = Just descr }
    where
      descr = fromString $ symbolVal (Proxy @sym)

instance (KnownSymbol sym, HasTemplatedLink b) => HasTemplatedLink (Summary sym :> b) where
  toTemplatedLink _ = let rl = toTemplatedLink (Proxy @b) in rl { _summary = Just summary }
    where
      summary = fromString $ symbolVal (Proxy @sym)



class HasLink endpoint => HasRelationLink endpoint where
  toRelationLink :: Proxy endpoint -> MkLink endpoint RelationLink

type RightLink b =
  ( HasRelationLink b
  , PolyvariadicComp (MkLink b RelationLink) (IsFun (MkLink b RelationLink))
  , Return (MkLink b RelationLink) (IsFun (MkLink b RelationLink)) ~ RelationLink
  , Replace (MkLink b RelationLink) RelationLink (IsFun (MkLink b RelationLink)) ~ MkLink b RelationLink
  )

instance (AllMime cts, ReflectMethod m) => HasRelationLink (Verb m s cts a) where
  toRelationLink _ = RelationLink
    { _segs = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = allMime (Proxy @cts)
    }

instance (AllMime cts, ReflectMethod m) => HasRelationLink (UVerb m cts as) where
  toRelationLink _ = RelationLink
    { _segs = mempty
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
    { _segs = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = mempty
    }

instance (ReflectMethod m, Accept ct) => HasRelationLink (Stream m s f ct a) where
  toRelationLink _ = RelationLink
    { _segs = mempty
    , _params = []
    , _fragment = Nothing
    , _templated = False
    , _method = reflectStdMethod (Proxy @m)
    , _summary = Nothing
    , _description = Nothing
    , _contentTypes = pure $ contentType (Proxy @ct)
    }

instance (KnownSymbol sym, RightLink b) => HasRelationLink ((sym :: Symbol) :> b) where
  toRelationLink _ = prependSeg seg ... toRelationLink (Proxy @b)
    where
      seg = fromString $ symbolVal (Proxy @sym)

instance (KnownSymbol sym, RightLink b) => HasRelationLink (Summary sym :> b) where
  toRelationLink _ = (\rl -> rl { _summary = Just summary }) ... toRelationLink (Proxy @b)
    where
      summary = fromString $ symbolVal (Proxy @sym)

instance (KnownSymbol sym, RightLink b) => HasRelationLink (Description sym :> b) where
  toRelationLink _ = (\rl -> rl { _description = Just descr }) ... toRelationLink (Proxy @b)
    where
      descr = fromString $ symbolVal (Proxy @sym)

instance HasRelationLink b => HasRelationLink (HttpVersion :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (BasicAuth realm userData :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance (KnownSymbol sym, RightLink b, ToHttpApiData a) => HasRelationLink (Capture' mods sym a :> b) where
  toRelationLink _ x = prependSeg (toUrlPiece x) ... toRelationLink (Proxy @b)

instance (KnownSymbol sym, RightLink b, ToHttpApiData a) => HasRelationLink (CaptureAll sym a :> b) where
  toRelationLink _ xs = prependSegs (toUrlPiece <$> xs) ... toRelationLink (Proxy @b)

instance (RightLink b, ToHttpApiData a) => HasRelationLink (Fragment a :> b) where
  toRelationLink _ x = (\l -> l { _fragment = Just $ toQueryParam x }) ... toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (Header' mods sym a :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance HasRelationLink b => HasRelationLink (IsSecure :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance (KnownSymbol sym, RightLink b) => HasRelationLink (QueryFlag sym :> b) where
  toRelationLink _ False = addParam (RelationParam (fromString $ symbolVal $ Proxy @sym) False (Just "false")) ... toRelationLink (Proxy @b)
  toRelationLink _ True  = addParam (RelationParam (fromString $ symbolVal $ Proxy @sym) False (Just "true")) ... toRelationLink (Proxy @b)

instance (KnownSymbol sym, ToHttpApiData a, RightLink b, SBoolI (FoldRequired mods)) => HasRelationLink (QueryParam' mods sym a :> b) where
  toRelationLink _ mv = addParam param ... toRelationLink (Proxy @b)
    where
      param = case sbool :: SBool (FoldRequired mods) of
        STrue  -> RelationParam (fromString $ symbolVal $ Proxy @sym) True    (Just $ toQueryParam mv)
        SFalse -> RelationParam (fromString $ symbolVal $ Proxy @sym) False $ toQueryParam <$> mv

instance (KnownSymbol sym, RightLink b, ToHttpApiData a) => HasRelationLink (QueryParams sym a :> b) where
  toRelationLink _ xs = addParams params ... toRelationLink (Proxy @b)
    where
      params = (\x -> RelationParam (fromString $ symbolVal $ Proxy @sym) False (Just $ toQueryParam x)) <$> xs

instance RightLink b => HasRelationLink (RemoteHost :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance RightLink b => HasRelationLink (ReqBody' mods cts a :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance RightLink b => HasRelationLink (WithResource res :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)

instance RightLink b => HasRelationLink (Vault :> b) where
  toRelationLink _ = toRelationLink (Proxy @b)
