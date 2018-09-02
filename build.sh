rm -rf dist/

cp -R static/ dist/
elm make --optimize --output=dist/elm.js src/Main.elm