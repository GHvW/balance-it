# balance-it
balance your check book that you totally still have and use


## Development
I'm using [elm-live](https://github.com/wking-io/elm-live) for development

With `elm-live` run `elm-live src/Main.elm --start-page=index.html --hot --open -- -- --output=index.js` to start a dev server with hot-reloading

Note: I had to use the two double dashes in the command - `-- --` - to get it to work. I'm not sure if its a Windows issue, or a npm global install issue, but if the `-- --` doesn't work before the `--output` section, try just a single set of `--`