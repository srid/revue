# revue

revue ([\ʁə.vy\\](https://en.wiktionary.org/wiki/revue#French)) is a Reflex based static website geared towards managing personal websites.

## Development

revue uses Obelisk. Use `ob run` to run locally, and `nix-build -A exe` (or `ob deploy`) for deployment.

## Roadmap

- [X] Use markdown (only in frontend)
- [ ] Retrieve markdown from the backend
- [ ] Extract markdown rendering as separate reflex-dom library
  - [ ] with Haskell syntax highlighting ([cf](https://github.com/mrkkrp/ghc-syntax-highlighter))
