# RCzechia    [![R-CMD-check](https://github.com/jlacko/RCzechia/workflows/R-CMD-check/badge.svg)](https://github.com/jlacko/RCzechia/actions)   [![Codecov test coverage](https://codecov.io/gh/jlacko/RCzechia/branch/master/graph/badge.svg)](https://codecov.io/gh/jlacko/RCzechia?branch=master) [![CRAN](http://www.r-pkg.org/badges/version/RCzechia)](https://cran.r-project.org/package=RCzechia) [![Downloads-total](http://cranlogs.r-pkg.org/badges/grand-total/RCzechia?color=brightgreen)](https://www.r-pkg.org:443/pkg/RCzechia) [![Downloads-weekly](http://cranlogs.r-pkg.org/badges/last-week/RCzechia?color=brightgreen)](https://www.r-pkg.org:443/pkg/RCzechia)
This project downloads a set of shapefiles relevant to the Czech Republic. It was inspired by the popular [`tigris`](https://github.com/walkerke/tigris) package for US datasets.  

<p align="center">
  <img src="https://github.com/jlacko/RCzechia/blob/master/data-raw/kraje-lo-res.png?raw=true" alt="Kraje České republiky"/>
</p>

For examples of RCzechia in action please see the package vignette:

* [Visualizing Czech Population](https://CRAN.R-project.org/package=RCzechia/vignettes/vignette.html#visualizing-czech-population)
* [Geocoding Locations & Drawing them on a Map](https://CRAN.R-project.org/package=RCzechia/vignettes/vignette.html#geocoding-locations-drawing-them-on-a-map)
* [Distance Between Prague and Brno](https://CRAN.R-project.org/package=RCzechia/vignettes/vignette.html#distance-between-prague-and-brno)
* [Geographical Center of the City of Brno](https://CRAN.R-project.org/package=RCzechia/vignettes/vignette.html#geographical-center-of-the-city-of-brno)
* [Interactive Map](https://CRAN.R-project.org/package=RCzechia/vignettes/vignette.html#interactive-map)
* [KFME Grid Cells - faunistické čtverce](https://CRAN.R-project.org/package=RCzechia/vignettes/vignette.html#kfme-grid-cells)
* [Terrain of the Czech Republic](https://CRAN.R-project.org/package=RCzechia/vignettes/vignette.html#terrain-of-the-czech-republic)


The administrative area polygons (except for city parts) are based on the [RÚIAN register](https://cs.wikipedia.org/wiki/Registr_%C3%BAzemn%C3%AD_identifikace,_adres_a_nemovitost%C3%AD); the natural objects (and city parts) are based on ArcČR 500 (https://www.arcdata.cz/produkty/geograficka-data/arccr-500) with some adjustments:  

 * the encoding was adjusted to handle Czech accents correctly in R  
 * coordinate reference system was changed from a local CRS ([S-JSTK](https://epsg.io/5513-1623)) to global WGS84 ([EPSG:4326](https://epsg.io/4326))   

The shapefiles are by necessity larger than the limits of a CRAN package size allow. The data are therefore stored remotely (on Amazon AWS) and downloaded as required. As consequence a working internet connection is required to fully use the package.

For the most commonly used shapes (*republika*, *kraje* and *okresy*) an optional low resolution version is also included. To access it specify the value of `resolution` parameter as `"low"` (default is `"high"`).

Using of the lo-res versions does not require a working internet connection. 

Access to the external files is logged, from time to time I check the logs (mainly to understand my bandwidth charges).

### A note to Czech users
Tohle je "oficiální", a tedy anglické, readme. Českou verzi naleznete na http://www.jla-data.net/cze/package-rczechia/

### Installation
The package is on CRAN (as of March 2018) so to get a stable version simply run:
```r 
install.packages("RCzechia")
```
You can also get the latest development version by running `remotes::install_github("jlacko/RCzechia")` and the last version built on [`sp`](https://github.com/edzer/sp) instead of [`sf`](https://github.com/r-spatial/sf) package by running  `remotes::install_github("jlacko/RCzechia", ref = "v0.1.4")`. 

### The following spatial objects are included:  
* **republika**: borders of the Czech Republic
* **kraje**: 14 regions of the Czech Republic + Prague.  
Key is KOD_CZNUTS3 (CZ NUTS3 code).
* **okresy**: 76 districts (LAU1 areas) of the Czech Republic + Prague (legally not *a district* but *the capital*).  
Key is KOD_LAU1 (CZ LAU1 code).
* **orp_polygony** 205 municipalities with extended powers (in Czech: obce s rozšířenou působností) + Prague (legally not *a city* but *the capital*).  
Key is KOD_ORP.
* **obce_polygony**: 6.258 municipalities of the Czech Republic.  
Key is KOD_OBEC, also contained are KOD_ORP (code of municipality with extended powers; see above) and KOD_POV (kód pověřené obce)
* **obce_body** the same as obce_polygony, but centroids instead of polygons.  
Key is again KOD_OBEC.
* **casti**: primarily 57 city parts of Prague, but also of other cities with defined parts (Brno, Ostrava..).  
Key is KOD.
* **reky**: streams and rivers
* **plochy**: stillwaters (lakes and ponds).
* **silnice**: roads (highways, speedways etc.)
* **zeleznice**: railroads
* **chr_uzemi**: protected natural areas (Chráněná území)
* **lesy**: woodland areas (more than 30 ha in area)
* **KFME_grid**: KFME grid cells (faunistické čtverce)
* **vyskopis**: terrain of the Czech republic as a {raster} package object

All objects are implemented as functions returning data frames, so must be followed by brackets (i.e. `hranice <- republika()`).

### In addition a number of utility functions is provided:  
* **geocode**: interfaces to geocoding API of [ČÚZK](https://cuzk.cz/en).
* **revgeo**: interfaces to reverse geocoding API of [ČÚZK](https://cuzk.cz/en).

