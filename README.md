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

## Example
```haskell
data User = User { usrId :: Int, addressId :: Int }
  deriving stock (Show, Eq)
  deriving anyclass ToJSON

data Address = Address { addrId :: Int, street :: String, number :: Int}
  deriving stock (Show, Eq)
  deriving anyclass ToJSON

type CompleteApi = AddressApi :<|> UserApi

type AddressApi = AddressGetOne
type AddressGetOne = "address" :> Capture "id" Int :> Get '[HALJSON] (Resource Address)

type UserApi = UserGetOne
type UserGetOne = "user" :> Capture "id" Int :> Get '[HALJSON] (Resource User)

instance HasResource CompleteApi User where
  toResource api u@(User uId aId) = Resource u
    [ ("self", mkSelf uId)
    , ("address", mkAddr (aId))
    ]
    where
      mkSelf = safeLink api (Proxy @UserGetOne)
      mkAddr = safeLink api (Proxy @AddressGetOne)
```
```haskell
>>> mimeRender (Proxy @HALJSON) $ toResource (Proxy @CompleteApi) $ User 1 42
```
```json
{
  "_links": {
    "address": {
      "href": "address/42"
    },
    "self": {
      "href": "user/1"
    }
  },
  "addressId": 42,
  "usrId": 1
}
```

## Goals
- [ ] Generically deriving `ToResource api` where possible
- [ ] Type-level rewriting of APIs like `CompleteAPI` to make API HATEOAS-compliant

## Media-Types
Currently we only serve Content-Type `application/hal+json`.
Support for others such as `application/vnd.collection+json` or `application/vnd.amundsen-uber+json` can easily be added
with instances for `Accept` and `MimeRender`.

Client usage with `MimeUnrender` is not yet supported but easily extensible.
