# Changelog

All notable changes to the servant-hateoas library will be documented in this
file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP versioning](https://pvp.haskell.org/).

## v0.2.2 _(2024-12-01)_

### Changed
- Removed `SomeToJSON` and replaced it with `SomeF f ToJSON` from `constrained-some`

## v0.2.1 _(2024-10-25)_

### Changed
- Export `CollectionItem`
- Improved documentation

## v0.2.0 _(2024-10-29)_

### Added
- Support for Content-Type `application/collection+json`
- Classes `EmbeddingResource res` & `CollectingResource res` for resource-modification

### Changed
- *(breaking change)* Renamed class `HasResource` to `Resource`, removed associated type and Content-Type param

## v0.1.1 _(2024-10-25)_

### Changed
- Improved documentation

## v0.1.0 _(2024-10-25)_

### Added
- Released
