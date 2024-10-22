![Static Badge](https://img.shields.io/badge/Lang-GHC2021-blue)
[![Haskell-CI](https://github.com/bruderj15/servant-hateoas/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bruderj15/servant-hateoas/actions/workflows/haskell-ci.yml)

# servant-hateoas
HATEOAS support for [servant](https://hackage.haskell.org/package/servant).

## State
This is not affiliated with the official `servant` maintainers.

Currently in infant state.
Final goal is something similar to what has been proposed [here](https://www.servant.dev/extending.html#other-directions).

## What can we do already?
Define an instance for class `ToResource api a` where `a` is the datatype you want to have a resty Api for and
`api` is the type of your Servant-Api within which the resty representation of your datatype `a` lives.

If you can provide some extra information then we can derive an instance for `ToResource api`
which includes a link to the self and all related single resources.
```haskell
-- | Datatypes that have a resty api.
class HasRestyApi a where
  -- | Servants Api-Type (endpoint) for fetching one value of the datatype by its identifier.
  type GetOneApi a :: Type

  -- | Type of the identifier for values of the datatype.
  type Id a :: Type

  -- | Function for getting the identifier from a value of the datatype.
  getId :: a -> Id a
```

## Example
```haskell
data User = User { userId :: Int, address :: Address }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, ToResource CompleteApi)

data Address = Address { addressId :: Int, street :: String, number :: Int}
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, ToResource CompleteApi)

instance HasRestyApi Address where
  type GetOneApi Address = AddressGetOne
  type Id Address = Int
  getId = addressId

instance HasRestyApi User where
  type GetOneApi User = UserGetOne
  type Id User = Int
  getId = userId

type CompleteApi = AddressApi :<|> UserApi

type AddressApi = AddressGetOne
type AddressGetOne = "address" :> Capture "id" Int :> Get '[HALJSON] (Resource Address)

type UserApi = UserGetOne
type UserGetOne = "user" :> Capture "id" Int :> Get '[HALJSON] (Resource User)
```
```haskell
>>> mimeRender (Proxy @HALJSON) $ toResource (Proxy @CompleteApi) $ User 1 $ Address 12 "Foo-Bar-Street" 42
```
```json
{
  "_links": {
    "self": {
      "href": "user/1"
    }
  },
  "address" :{"
    addressId":12,
    "number":42,
    "street": "Foo-Bar-Street"
  },
  "userId": 1
}
```

## Media-Types
Currently we only serve Content-Type `application/hal+json`.
Support for others such as `application/vnd.collection+json` or `application/vnd.amundsen-uber+json` can easily be added
with instances for `Accept` and `MimeRender`.

This would not affect generic link generation for related entities besides adding information regarding HATEOAS-props such as `actions`.
Client usage with `MimeUnrender` is not yet supported but easily extensible.
