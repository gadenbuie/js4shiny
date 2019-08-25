## Building `js4shiny` package assets

### tachyons-style color utility CSS classes

[tachyons]: https://tachyons.io

The `build_sass.R` file builds `.sass` or `.scss` assets in this package. Currently this is limited to `colors.css`, a collection [tachyons]-style of color-related utility CSS classes.

### js4shiny xaringan theme

The [js4shiny-xaringan-theme.R](js4shiny-xaringan-theme.R) script uses [xaringanthemer](https://pkg.garrickadenbuie.com/xaringanthemer) to build the `js4shiny-xaringan-base.css` styles. (Note: I used the in-development version of xaringanthemer.)

### Customized tachyons build

A customized [tachyons] CSS build is included with `js4shiny` and can be built via the following steps, starting from within this directory.

```bash
# clone tachyons-custom
git clone https://github.com/tachyons-css/tachyons-custom.git
cd tachyons-custom

# copy js4shiny _variables.css into src/ 
cp ../../inst/css/_variables.css src/_variables.css

# install deps and build tachyons
npm install
npm run build

# copy tachyons into xaringan template directory
cp css/tachyons.min.css ../../inst/rmarkdown/templates/js4shiny-xaringan/skeleton/assets/css/tachyons.min.css
```
