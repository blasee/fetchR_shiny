This is the repo for a shiny version of the
[fetchR](https://github.com/blasee/fetchR) package.

See the web application
[here](https://blasee.shinyapps.io/fetchR_shiny/).

You can run this application locally by installing the **shiny** package
in R, and use the `runGitHub()` function:

    if (!require('shiny')) install.packages("shiny")
    shiny::runGitHub("blasee/fetchR_shiny")

Or you can clone or download this repository, and use the `runApp()`
function:

    shiny::runApp("fetchR_shiny")
