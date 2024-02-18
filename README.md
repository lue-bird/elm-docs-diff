## elm-project-metadata-diff

Compare versions of an API using their `docs.json` metadata which can be fetched online or selected from a file.

→ [website](https://lue-bird.github.io/elm-docs-json-diff/)

The idea is for this to be a more general `elm diff` to show changes between for example
  - versions of a package that does't have a complete change log
  - the original package and a fork or re-publish under a different name
  - versions of private packages

### run locally
```noformatingples
npx elm-watch hot
```
  - [lydell/elm-watch website](https://lydell.github.io/elm-watch/)

and open the index.html file or better
```noformatingples
serve
```
  - [vercel/serve](https://github.com/vercel/serve)

## TODO
  - a way to generate a textual form like `elm diff` that can for example be used to fill in missing change log items
