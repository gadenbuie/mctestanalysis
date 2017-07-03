# MC Test Analysis

This [R] package provides a nice set of interfaces via [Shiny] for the analysis and review of multiple choice tests.
View full package documentation at <https://gadenbuie.github.io/mctestanalysis/>.

## Try Online

You can try out the Shiny web interface online at <https://gadenbuie.shinyapps.io/mctestanalysis/>, which demonstrates the test results explorer interface but cannot produce the PDF or HTML reports.
For the complete features, you can install the package and use the interface locally (without have to upload your data) by following the instructions below.

## Table of contents

- [Required Software](#required-software)
    - [R](#r)
    - [RStudio](#rstudio)
    - [LaTeX](#latex)
- [Installation](#installation)
- [Usage](#usage)
- [Data Format](#data-format)

# Required Software

You need to download and install the latest version of the following software.
If you already have recent versions of R, RStudio and LaTeX (optional) installed, you can skip ahead to the section on [installing the MCTestAnalysis library](#install-mctestanalysis).

## R 

Using the MCTestAnalysis package does not require any R programming. 
If you are interested in learning to program in R, RStudio have collated some excellent resources for [Learning R Programming](https://www.rstudio.com/online-learning/#R) on their webpage.

To install:

- <http://cran.r-project.org>

- Click the "Download R for ..." link for your OS (Linux, Mac, Windows)

- Click the link for the **base** distribution. This package was built and tested on R 3.3.2, although both this package and R may have been updated.


## RStudio

[RStudio] is an IDE (an application that supports the development and use of a language) for R.
The MCTestAnalysis package is best used from within RStudio, but the user only needs to be able to find the Console window to run the few commands [described below](#install-mctestanalysis).

For a quick introduction and reference for the RStudio IDE, RStudio publishes an up-to-date [RStudio cheatsheet](https://www.rstudio.com/resources/cheatsheets/).
The [RStudio Essentials Webinar Series](https://www.rstudio.com/resources/webinars/rstudio-essentials-webinar-series-part-1/) is also a good place to start learning about the RStudio IDE.

To install:

- <http://www.rstudio.com/products/rstudio/download>

- Choose the appropriate installer for your OS under the **Installers for Supported Platforms** heading.

## LaTeX

LaTeX is required in order to create PDF reports.
The package will fall back on HTML reports if `pdflatex` is not installed.
The HTML version of the report works well on screens but does not print nicely.

If you'd like to output the report as well-behaved, nicely-printing PDF, a LaTeX distribution is required.

To install:

- Windows (MiKTeX)
     - Install the "Basic MiKTeX Installer" from <https://miktex.org/download>.
     - *Optional*: After installation, run [the update wizard](https://miktex.org/howto/update-miktex) by running "MiKTeX Update" from the Windows start menu.

- Mac OS X (MacTeX)
    - Install the full MacTeX installation from <http://www.tug.org/mactex/>. (Warning: the download is big!)

- Ubuntu (texlive)
    - Install texlive via the package manager or on the command line with `sudo apt-get install texlive-core`.
    - Also install fonts and extra things with `sudo apt-get install texlive-fonts-recommended texlive-latex-recommended`.
    - Or alternatively install the full texlive package with `sudo apt-get install texlive-latex-extra`.


# Installation

## Mac/Linux

On Mac OS X or Unix-based systems, install this package by running the following commands inside [RStudio] or the [R] command line:

```r
install.packages("devtools")
devtools::install_github("gadenbuie/mctestanalysis")
```

## Windows

On Windows, the current version of `devtools` doesn't correctly install all package dependencies, so they need to be installed manually before installing the MCTestAnalysis package.

Copy and paste the following command into the RStudio console or the R command line to install the MCTestAnalysis package dependencies.

```r
install.packages(c("devtools", "dplyr", "ggplot2", "shiny", "miniUI",
                   "ltm", "psych", "psychometric", "reshape2", "tibble",
                   "DT", "rmarkdown", "backports", "pander", "gridExtra",
                   "survival"))
```

Then run the following to complete the installation of the MCTestAnalysis package.

```r
devtools::install_github("gadenbuie/mctestanalysis")
```

# Usage

Open [RStudio] or [R] on the command line or GUI and run the following commands:

```r
library(MCTestAnalysis)

# Launch test exploration interface
explore()

# Create a test analysis report
report()
```

When running `explore()` a browser window will open with the MC Test Analysis exploration application.
The `report()` function will launch a window within RStudio to guide the user through the creation of a PDF report.

# Data format

This package requires both an Answer Key and a table of student responses. 
An example of each table is provided, in the preferred format, in the  [`inst/extdata` folder](inst/extdata) of this repo, or from links on the "Import" tab of the `explore()` or `report()` interfaces.

A detailed overview of the required data format is available at <http://www.eng.usf.edu/~kaw/MCTestAnalysis/MCTestAnalysis_input.pdf>.
Additional example [test results](http://www.eng.usf.edu/~kaw/MCTestAnalysis/sample_answer_key.csv) and [answer key](http://www.eng.usf.edu/~kaw/MCTestAnalysis/sample_student_answers.csv) CSV files are also available.

[R]: https://cran.r-project.org/
[Shiny]: http://shiny.rstudio.com/
[Rstudio]: https://www.rstudio.com/products/RStudio/
[Rtools]: https://cran.r-project.org/bin/windows/Rtools/
