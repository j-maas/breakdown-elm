rm -rf dist/

cp -a static/. dist/
cp -a favicon/. dist/
elm make --optimize --output=dist/elm.js src/Main.elm