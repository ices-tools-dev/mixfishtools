<img src="hexSticker/hexSticker_mixfishtools_trans.png" width="250"/>

# mixfishtools

[![:registry status badge](https://ices-tools-prod.r-universe.dev/badges/mixfishtools)](https://ices-tools-prod.r-universe.dev)

**To install:**

```         
# install pre-compiled package (preferred)
install.packages('mixfishtools', repo = 'https://ices-tools-prod.r-universe.dev')

# install from source
library(remotes)
install_github(repo = "ices-tools-dev/mixfishtools")
```

# Vignettes

-   [***mixfishtools guide***](https://raw.githack.com/ices-tools-dev/mixfishtools/main/doc/mixfishtoolsGuide.html) - Guide to use of the *mixfishtools* functions.

-   [***FLStock creation***](https://raw.githack.com/ices-tools-dev/mixfishtools/main/doc/FLStock2WGMIXFISH.html) - Guide to creating FLStock objects for use in WGMIXFISH

-   [***Reproduce advice***](https://raw.githack.com/ices-tools-dev/mixfishtools/main/doc/reproduceAdvice.html) - WGMIXFISH quality control procedure to test the reproduction of single stock advice forecasts before proceeding with mixed fishery modelling scenarios

To build directly during installation: `remotes::install_github(repo = "ices-tools-dev/mixfishtools", build_vignettes = TRUE)`, followed by `browseVignettes(package = "mixfishtools")` to view.

Both Rmarkdown documents to produce the vignettes are found in the '/vignettes' subfolder of the repo. When using the cloned repo, these can be built using `devtools::build_vignettes()`.
