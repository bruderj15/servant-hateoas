[![Hackage](https://img.shields.io/hackage/v/servant-hateoas.svg)](https://hackage.haskell.org/package/servant-hateoas)
![Static Badge](https://img.shields.io/badge/Lang-GHC2021-blue)
[![Haskell-CI](https://github.com/bruderj15/servant-hateoas/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bruderj15/servant-hateoas/actions/workflows/haskell-ci.yml)

# servant-hateoas
HATEOAS support for [servant](https://hackage.haskell.org/package/servant).

## State
This is not affiliated with the official `servant` maintainers.

Currently in infant state.
Final goal is something similar to what has been proposed [here](https://www.servant.dev/extending.html#other-directions).

## What can we do already?
Define an instance for class `ToResource ct api a` where `ct` is the Content-Type, `a` is the datatype you want to have a resty Api for and
`api` is the type of your Servant-Api within which the resty representation of your datatype `a` lives.

When providing some extra information with an instance for `Related a` we can derive related links.
## Example
```haskell
data User = User { usrId :: Int, addressId :: Int, income :: Double }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass ToJSON

data Address = Address { addrId :: Int, street :: String, number :: Int}
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass ToJSON

type CompleteApi = AddressApi :<|> UserApi

type AddressApi = AddressGetOne
type AddressGetOne = "address" :> Capture "id" Int :> Get '[HAL JSON] (HALResource Address)

type UserApi = UserGetOne :<|> UserGetAll
type UserGetOne = "user" :> Capture "id" Int :> Get '[HAL JSON] (HALResource User)
type UserGetAll = "user" :> Get '[HAL JSON] (HALResource [User])

instance Related User where
  type IdSelName User      = "usrId"              -- This is type-safe because of using class HasField
  type GetOneApi User      = UserGetOne
  type CollectionName User = "users"
  type Relations User      =
    '[ 'HRel "address" "addressId" AddressGetOne  -- Also type-safe
     ]
```
```haskell
>>> mimeRender (Proxy @JSON) $ toResource (Proxy @(HAL JSON)) (Proxy @CompleteApi) $ User 1 100 100000
```
```json
{
  "_links": {
    "address": {
      "href": "address/100"
    },
    "self": {
      "href": "user/1"
    }
  },
  "addressId": 100,
  "income": 100000,
  "usrId": 1
}
```

## Goals
- [x] Deriving links where possible
- [ ] Deriving links for paging, ...
- [ ] Type-level rewriting of APIs like `CompleteAPI` to make API HATEOAS-compliant

## Media-Types
Currently we only serve Content-Type `application/hal+json`.
Support for others such as `application/vnd.collection+json` or `application/vnd.amundsen-uber+json` can easily be added
with instances for `Accept` and `MimeRender`.

Client usage with `MimeUnrender` is not yet supported but easily extensible.
