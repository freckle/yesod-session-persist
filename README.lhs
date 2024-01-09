# haskell-library-template

[![Hackage](https://img.shields.io/hackage/v/haskell-library-template.svg?style=flat)](https://hackage.haskell.org/package/haskell-library-template)
[![Stackage Nightly](http://stackage.org/package/haskell-library-template/badge/nightly)](http://stackage.org/nightly/package/haskell-library-template)
[![Stackage LTS](http://stackage.org/package/haskell-library-template/badge/lts)](http://stackage.org/lts/package/haskell-library-template)
[![CI](https://github.com/freckle/haskell-library-template/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/haskell-library-template/actions/workflows/ci.yml)

_Synopsis_

## Example

<!--
```haskell
module Main (main) where

import Prelude
import Text.Markdown.Unlit ()
```
-->

```haskell
someExample :: IO ()
someExample = putStrLn "Hello world"
```

<!--
```haskell
main :: IO ()
main = someExample
```
-->

## Development & Tests

```console
stack build --fast --pedantic --test --file-watch
```

---

## How to use this Template

Haskell library template used at Freckle.

### Create your repo

```sh
gh repo create --template freckle/haskell-library-template --public freckle/<name>
git clone git@github.com:freckle/<name>
cd ./<name>
```

### Rename your package

```sh
find -type f -exec \
  sed -i s/haskell-library-template/my-name/ {} +
```

Edit `package.yaml` as necessary.

### Enable release

When you are ready to release your library, simply remove the conditional from
the release workflow.

```diff
-      - if: false # Remove when ready to release
```

### Open repo up to [hacktoberfest][hacktoberfest] contributions

Add the `hacktoberfest` topic to your repo if

- you're planning on releasing it as open source, and
- you think it would benefit from and be amenable to public contributions

[hacktoberfest]: https://hacktoberfest.digitalocean.com/

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
