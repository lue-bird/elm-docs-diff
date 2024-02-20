## elm-docs-diff

Compare versions of an API using their `docs.json`. It's both

  - a [website](https://lue-bird.github.io/elm-docs-diff/) that allows you to select local `docs.json` files or fetch them online and show a nice diff
  - an unopinionated package that exposes `Elm...Diff` helpers for [packages, modules, declarations and types](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Docs)
    in case you want to use the diff in another context like [elm-review](https://dark.elm.dmy.fr/packages/jfmengels/elm-review/latest/) or [elm-posix](https://dark.elm.dmy.fr/packages/albertdahlin/elm-posix/latest/)

The idea is for this to be a more general `elm diff` to show changes between for example
  - versions of a package that doesn't have a complete change log
  - the original package and a fork or re-publish under a different name
  - versions of private packages

## thanks
  - rlefevre for [elm.dmy.fr](https://github.com/dmy/elm.dmy.fr) which the diff website uses as a proxy because it allows fetching package `docs.json`
