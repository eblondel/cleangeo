cleangeo
========
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/cleangeo)](https://cran.r-project.org/package=cleangeo)
[![Github_Status_Badge](https://img.shields.io/badge/Github-0.3-blue.svg)](https://github.com/eblondel/cleangeo)
[![cran checks](https://badges.cranchecks.info/worst/cleangeo.svg)](https://cran.r-project.org/web/checks/check_results_cleangeo.html)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.822775.svg)](https://doi.org/10.5281/zenodo.822775)

``cleangeo`` provides utilities to clean geometries from spatial objects in R. If you aim to do geoprocessing in R, this package will help you in cleaning your ``Spatial*`` objects before, and avoid geoprocessing errors later.

### Note on __cleangeo archiving process__ (after version 0.3), needs/prerequisites to move to `sf`

The_ _cleangeo_ package was developed in 2015 at at time where _sp_ and _rgeos_, _maptools_, _raster_, etc. were among R solutions to deal with spatial data handling. With the increase of _sf_ instead of _sp_ objects, it's recommended to switch progressively to _sf_ as base for handling spatial features, and to use `sf::st_make_valid` to validate geometries (which is now the default in cleangeo). Following the archiving of _rgeos_, _maptools_, it is probable that _cleangeo_ may also be archived.

Native geometry fixing strategies, although applicable in some cases, do not cover the full typology of geometry issues we may find with vector data. Although refactored to remove the dependency with _rgeos_ and _maptools_, some unexpected behaviors can be found. 

With the progress done on validating geometries, especially with _sf_, the default strategy to fix geometries in _cleangeo_ has now been switched to the use of `sf::st_make_valid`. 

Nonetheless, although the original set-up and actual name of _sf_ refers to an __ISO/OGC specification__ (Simple Features), users that handle geospatial data with tools that rely on the ISO/OGC realm (__most of GIS tools, from the OsGeo or even proprietary software__) should use _sf_ __cautiously__ as the default behavior doesn't rely anymore on the ISO/OGC realm for managing geographic coordinates (it was initially) but on a non-standard technology-specific geometry model handled with _s2_ (from the name of the library developed by Google). To turn off this default set-up, and to conserve legacy behaviors (backward-compatibility) in geospatial processes, users will need to ensure they use `sf::sf_use_s2(FALSE)`. This is particularly needed if you grab data from OGC data services (eg. with _ows4R_), and related OGC formats that do not rely on the _s2_ data model, but on the ISO/OGC Simple Features data model.

### Citation

We thank in advance people that use ``cleangeo`` for citing it in their work / publication(s). For this, please use the citation provided at this link [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.822775.svg)](https://doi.org/10.5281/zenodo.822775)

### Use cases

Some projects using ``cleangeo``:

* [Ocean Health Index](https://github.com/OHI-Science)
* [UNHCR Iraq Information Management](https://github.com/unhcr-iraq)
* [UN-FAO Fisheries Global Information System (FIGIS)](https://github.com/openfigis)
* [EU FP7 CityPulse Project](https://github.com/CityPulse)
