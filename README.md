# MC Test Analysis

This [R] package provides a web interface via [Shiny] for the analysis and
review of multiple choice tests.

**Warning:** The package is currently in development; features may change or
break without warning.

# Installation and Use

Open [RStudio] or [R] on the command line or GUI and run the following commands:

```
install.packages("devtools")
devtools::install_github("gadenbuie/mctestanalysis")
library(MCTestAnalysis)
run_app()
```

A browser window will open with the MC Test Analysis application.

# Data format

This package requires both an Answer Key and a table of student responses. An
example of each table is provided, in the preferred format, in the 
[`inst/extdata` folder](inst/extdata]) of this repo, or from links on the
"Import" tab of the included Shiny App.

[R]: https://cran.r-project.org/
[Shiny]: http://shiny.rstudio.com/
[Rstudio]: https://www.rstudio.com/products/RStudio/
