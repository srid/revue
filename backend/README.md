
- The `page` symlink here, pointing to `../page`, is to workaround `ob run` and `nix-build -A exe` using different current working directory to the point of breaking `file-embed`'s behaviour.
