# revue

revue ([\ʁə.vy\\](https://en.wiktionary.org/wiki/revue#French)) is a Reflex based  CMS geared towards managing personal websites.

## Development

revue uses Obelisk. Use `ob run` to run locally, and `nix-build -A exe` (or `ob deploy`) for deployment.

## Documentation

Add your markdown files (must have a `landing.md` file) to a directory, and run as:

```
export REVUE_CONTENT_DIR=/path/to/your/content 
ob run
```


Each page automatically gets its own route. For example, the `hello.md` content can be accessed from the frontend as `http://localhost:8000/page/hello`

## Example

See [srid.ca](https://github.com/srid/srid.ca) for an example site, which includes systemd and nginx configuration.

## Roadmap

https://github.com/srid/revue/projects/1?add_cards_query=is%3Aopen
