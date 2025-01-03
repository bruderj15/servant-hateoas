[![Hackage](https://img.shields.io/hackage/v/servant-hateoas.svg)](https://hackage.haskell.org/package/servant-hateoas)
![Static Badge](https://img.shields.io/badge/Lang-GHC2021-blue)
[![Haskell-CI](https://github.com/bruderj15/servant-hateoas/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bruderj15/servant-hateoas/actions/workflows/haskell-ci.yml)

# servant-hateoas
HATEOAS support for [servant](https://hackage.haskell.org/package/servant).

Infant state, highly experimental.

Find a motivating example further down this README.

## What can we do already?
- [x] Derive a layered HATEOAS-API and a server-implementation from an API, basically what has been touched on [here](https://www.servant.dev/extending.html#other-directions).
- [x] Derive a HATEOAS-API from an API by rewriting the API and its server-implementation
  - [x] Wrapping the response types of your API with Resource-Representations
  - [x] Automatically adding the self-link to every resource
  - [x] Adding custom links to resources via instances for type-class `ToResource`
- [x] Directly write a HATEOAS-API yourself

## What can we do better?
Deriving the layered HATEOAS-API from your API does not require your API to be structured in a certain way.

However, for rewriting your API we need you to specify your server-implementation as an instance of class `HasHandler` (bad name, should be `HasServer` - exists already).

This currently makes it tricky for APIs which have shared path segments, e.g. `"api" :> (UserApi :<|> AddressApi)`

Therefore we currently need an instance on each flattened endpoint of the API, e.g. for `"api :> UserApi"` and `"api :> AddressApi"`.

## What's on the horizon?
A lot. There are plenty of opportunities.
- [ ] Merging the derived HATEOAS Layer-API with the rewritten HATEOAS API.
- [ ] Automatically adding links for [servant-pagination](https://hackage.haskell.org/package/servant-pagination)
- [ ] Adding rich descriptions for Hypermedia-relations for content-types such as `application/prs.hal-forms+json`
- [ ] ...

## Media-Types
- [x] `application/hal+json`
- [ ] `application/vnd.collection+json`: Work in progrress
- [ ] `application/prs.hal-forms+json`: Soon
- [ ] Others: Maybe

Client usage with `MimeUnrender` is not yet supported.

## Example

Suppose we have users and addresses, where each user has an address:
```haskell
data User = User { usrId :: Int, addressId :: Int, income :: Double }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON)

data Address = Address { addrId :: Int, street :: String, city :: String }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON)
```

We need to define how their resource-representation looks like:
```haskell
-- default just wrapps an address to a resource
instance ToResource res Address

-- add a link to the address-resource with the relation "address" for the user-resource
instance Resource res => ToResource res User where
  toResource _ ct usr = addRel ("address", mkAddrLink $ addressId usr) $ wrap usr
    where
      mkAddrLink = toRelationLink $ resourcifyProxy (Proxy @AddressGetOne) ct
```

Further we define our API as usual:
```haskell
type Api = UserApi :<|> AddressApi

type UserApi = UserGetOne :<|> UserGetAll :<|> UserGetQuery
type UserGetOne    = "api" :> "user" :> Title "The user with the given id" :> Capture "id" Int :> Get '[JSON] User
type UserGetAll    = "api" :> "user" :> Get '[JSON] [User]
type UserGetQuery  = "api" :> "user" :> "query" :> QueryParam "addrId" Int :> QueryParam "income" Double :> Get '[JSON] User

type AddressApi = AddressGetOne
type AddressGetOne = "api" :> "address" :> Capture "id" Int :> Get '[JSON] Address
```

Getting all the layers of the API in a HATEOAS way now is as simple as:
```haskell
layerServer :: Server (Resourcify (MkLayers Api) (HAL JSON))
layerServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @(MkLayers Api))
```

If we further want to rewrite our API to a HATEOAS-API, we need to define the server-implementation as an instance of `HasHandler`.

This is nothing but the usual servant-server implementation, just that the implementation is not floating around in the source code and instead is bound to a class instance.
```haskell
instance HasHandler UserGetOne where
  getHandler _ _ = \uId -> return $ User uId 0 0
instance HasHandler UserGetAll where
  getHandler _ _ = return [User 1 1 1000, User 2 2 2000, User 42 3 3000]
instance HasHandler UserGetQuery where
  getHandler _ _ = \mAddrId mIncome -> return $ User 42 (maybe 0 id mAddrId) (maybe 0 id mIncome)
instance HasHandler AddressGetOne where
  getHandler _ _ = \aId -> return $ Address aId "Foo St" "BarBaz"
```

Getting the rewritten HATEOAS-API and it's server-implementation is as simple as:
```haskell
apiServer :: Server (Resourcify Api (HAL JSON))
apiServer = getResourceServer (Proxy @Handler) (Proxy @(HAL JSON)) (Proxy @Api)
```

For now `apiServer` and `layerServer` exist in isolation, but the goal is to merge them into one.

When we now run the `layerServer` and request `GET http://host:port/api/user/query`, we get:
```json
{
    "_embedded": {},
    "_links": {
        "addrId": {
            "href": "/api/user/query{?addrId}",
            "templated": true,
            "type": "application/hal+json"
        },
        "income": {
            "href": "/api/user/query{?income}",
            "templated": true,
            "type": "application/hal+json"
        },
        "self": {
            "href": "/api/user/query",
            "type": "application/hal+json"
        }
    }
}
```

Similar for `userServer` and `GET http://host:port/api/user/42`:
```json
{
    "_embedded": {},
    "_links": {
        "address": {
            "href": "/api/address/0",
            "type": "application/hal+json"
        },
        "self": {
            "href": "/api/user/42",
            "title": "The user with the given id",
            "type": "application/hal+json"
        }
    },
    "addressId": 0,
    "income": 0,
    "usrId": 42
}
```

The complete example can be found [here](https://github.com/bruderj15/servant-hateoas/blob/main/src/Servant/Hateoas/Example.hs).

## Contact information
Contributions, critics and bug reports are welcome!

Please feel free to contact me through GitHub.
