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
Define an instance for class `ToResource api res a` where `api` is the type of your Servant-Api within which the resty representation
of your datatype `a` lives and `res` is the resource-representation to create.

When providing some extra information with an instance for `Related a` there are stock instances which derive the links
based on the relations in your instance.
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
type UserGetAll = "user" :> Get '[Collection JSON] (CollectionResource User)

instance Related User where
  type IdSelName User = "usrId"
  type GetOneApi User = UserGetOne
  type CollectionName User = "users"
  type Relations User =
    '[ 'HRel "address" "addressId" AddressGetOne
     ]

```
```haskell
>>> mimeRender (Proxy @(HAL JSON)) $ toResource @CompleteApi @HALResource $ User 1 42 100000
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
  "income": 100000,
  "usrId": 1
}
```

## Goals
- [x] Deriving simple links for self and relations
- [ ] Deriving links for paging, ...
- [ ] Type-level rewriting of APIs like `CompleteAPI` to make API HATEOAS-compliant

## Media-Types
- [x] `application/hal+json`
- [x] `application/collection+json`
- [ ] Others: Easily extensible

Client usage with `MimeUnrender` is not yet supported.

## Contact information
Contributions, critics and bug reports are welcome!

Please feel free to contact me through GitHub.
