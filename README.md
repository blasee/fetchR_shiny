Calculate wind fetch: The shiny version
=======================================

The [**fetchR**](https://github.com/blasee/fetchR) package is designed to calculate fetch lengths at locations anywhere on Earth, using **R**. This shiny version of **fetchR** allows users to make these calculations *without* requiring **R** by using the [online web application](https://blasee.shinyapps.io/fetchR_shiny/).

Deploy this application locally
-------------------------------

You can run this application locally by installing the **shiny** package, and use the `runGitHub()` function:

``` r
if (!require('shiny')) 
  install.packages("shiny")

shiny::runGitHub("blasee/fetchR_shiny")
```

Or you can clone or download this repository, and use the `runApp()` function:

``` r
shiny::runApp("fetchR_shiny")
```

How to use the **fetchR** web application
=========================================

The [**fetchR**](https://github.com/blasee/fetchR) web application requires two shapefiles; one for the coastlines and other boundaries, and one for the locations at which to calculate wind fetch. The following example details the steps required for calculating fetch with a reproducible example using data from the [Land Information New Zealand (LINZ) Data Service](https://data.linz.govt.nz/layer/1153-nz-coastlines-and-islands-polygons-topo-150k/)[1].

1) Upload a polygon shapefile
-----------------------------

Download the [high resolution New Zealand coastlines and islands polygons shapefile](https://data.linz.govt.nz/layer/1153-nz-coastlines-and-islands-polygons-topo-150k/) as a GIS shapefile with the NZGD2000 / New Zealand Transverse Mercator 2000 (EPSG:2193).

2) Upload a points shapefile
----------------------------

3) Set maximum distance and number of directions
------------------------------------------------

<!-- p("Steps:"), -->
<!--                  tags$ol( -->
<!--                    tags$li(p(strong("Upload a polygon shapefile"), -->
<!--                              p("This layer represents the coastline, islands and any other obstruction to wind. Each obstruction needs to be represented by a separate polygon within the polygon shapefile."))), -->
<!--                    tags$li(p(strong("Upload a point shapefile")), -->
<!--                            p("This layer represents the individual locations at which the wind fetch is to be calculated. Each location needs to be represented by a point within a point (or multipoint) shapefile.")), -->
<!--                    tags$li(p(strong("Set the maximum distance"))), -->
<!--                    tags$li(p(strong("Set the number of directions to calculate")), -->
<!--                            p("This is the number of fetch directions to calculate per quadrant (90 degrees).")), -->
<!--                    tags$li(p(strong("Calculate fetch")), -->
<!--                            p()) -->
<!--                  ) -->

[1] This requires a (free) [registration to the LINZ Data Service](https://data.linz.govt.nz/accounts/register/) and acceptance of the [terms of conditions](https://data.linz.govt.nz/terms-of-use/) and [privacy policy](https://data.linz.govt.nz/privacy-policy/). The data sourced from Land Information New Zealand has not been adapted and is protected under CC-By Land Information New Zealand.
