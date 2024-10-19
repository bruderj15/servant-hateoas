![Static Badge](https://img.shields.io/badge/Lang-GHC2021-blue)
[![Haskell-CI](https://github.com/bruderj15/servant-hateoas/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/bruderj15/servant-hateoas/actions/workflows/haskell-ci.yml)

# servant-hateoas

This is not related to the official `servant` maintainers.
Currently in infant state, final goal is something similar to what has been proposed [here](https://www.servant.dev/extending.html#other-directions).

The plan is as follows:
- [x] Class `ToResource api a` for generating RESTy Resource-Representation with links to related resources
- [x] Manual instances for `ToResource api a`
- [ ] Generic instance for `ToResource api a`

This still requires us to list the endpoint:
```haskell
data User = ... deriving (Generic, ToResource api)
type UserApi = "user" :> Capture "id" Int :> Get '[HALJSON] (Resource User)

userServer :: Server UserApi
userServer = \i -> return $ toResource (Proxy @UserApi) $ findUser ...
```
This is already good.

But: Ideally we transform
```haskell
type UserApi = "user" :> Capture "id" Int :> Get '[JSON] User
```
into
```haskell
type RestyUserApi = UserApi
               :<|> "user" :> Capture "id" Int :> Get '[HALJSON] (Resource User)
```
and then generate
```haskell
restyUserServer :: Server RestyUserApi
restyUserServer = ...
```

Currently we only serve Content-Type `application/hal+json`.
Support for others such as `application/vnd.collection+json` or `application/vnd.amundsen-uber+json` can easily be added
with instances for `Accept` and `MimeRender`.
This would not affect generic link generation for related entities besides adding information regarding HATEOAS-props such as `actions`.
Client usage with `MimeUnrender` is not yet supported but easily extensible.
