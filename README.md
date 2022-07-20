# elm-example-app
This project represents in a step-by-step (commit-by-commit) manner how to develop a sandbox-level `Elm` SPA

## Set-up and looking around
To compile a `main.js` file (which is connected to `index.html`) you should run
```sh
elm make --debug src/Main.elm --output="./main.js"
```  

If you change `App.elm`, you should run the same script again to compile a new version of the `main.js` file. 
