rm -rf dist/

cp -R static/ dist/
cp -R favicon/ dist/
elm make --optimize --output=dist/elm.js src/Main.elm