#
# https://cran.r-project.org/web/packages/V8/vignettes/npm.html
#
# https://github.com/Leonidas-from-XIV/node-xml2js
# Marek Kubica, MIT Licence
module="xml2js"
cd inst/js
npm install $module
echo "global.injs = require('$module');" > in.js
browserify in.js -o bundle.js
minify -o bundle.js bundle.js
rm -rf node_modules
rm package-lock.json
rm package.json
rm in.js
