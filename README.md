# revue

revue ([\ʁə.vy\\](https://en.wiktionary.org/wiki/revue#French)) is a Reflex based static website geared towards managing personal websites.

## Development

revue uses Obelisk. Use `ob run` to run locally, and `nix-build -A exe` (or `ob deploy`) for deployment.

## Documentation

Markdown pages can be added to `page` directory, for example:

```
echo "This is _markdown_ content" > page/hello.md
```

These pages get embedded automatically during Haskell compilation. Each page automatically gets its own route. For example, the `page/hello.md` content can be accessed from the frontend as `http://localhost:8000/hello` (append `.md` to the URL to view the markdown source).

## Roadmap

https://github.com/srid/revue/projects/1?add_cards_query=is%3Aopen
