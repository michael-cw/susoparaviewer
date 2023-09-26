
<!-- README.md is generated from README.Rmd. Please edit that file -->

<a href='https://docs.mysurvey.solutions/'><img src="man/figures/susotools.png" align="right" height="139" style="float:right; height:139px;"/></a>

# Survey Solutions Paradata Viewer Application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<div align="justify">

The Survey Solutions Paradata Viewer Application[^1] is part of a
comprehensive toolkit designed to enhance the process of survey
implementation through [Survey
Solutions](https://docs.mysurvey.solutions/). The underlying application
allows you to visualize Survey Solutions paradata, at the questionnaire,
the interviewer and the geographic level.

#### Key Features

1.  **Visualization at the questionnaire level**: Review a
    questionnaire’s average time profile, average timings and number of
    invalid questions.

2.  **Visualization at the interviewer level**: Review average
    completion times, average pace and number of response removals at
    the level of the interviewer

3.  **Visualization at the geographic level**: Review average completion
    times, average pace and number of response removals by geographic
    regions[^2]. Visualization is either through a
    [*mapdeck*](https://cran.r-project.org/web/packages/mapdeck/vignettes/mapdeck.html)
    (requires an API key) or a
    [*leaflet*](https://rstudio.github.io/leaflet/) map.

4.  **Admin Interface**: This intuitive interface lets you:

    - Directly connect to your Survey Solutions server to load the
      paradata.
    - Specify the time interval when new data should be downloaded.

5.  **Flexibility in data input**: Data can be provided as:

    - File upload.
    - Directly from the server through the API.
    - From a local directory.

6.  **Download the data**: The application also allows to dowload the
    data in a more convenient format and by event type.

7.  **Direct review of interviews/interviewers**: Allows you to directly
    connect to the Survey Solutions server and review interviews and
    interviewers.

8.  **Report creation**: Generates a full paradata report as a word
    document at the:

    - *Questionnaire* level,
    - *Interviewer* level,
    - *Map* level[^3].

## Installation

1.  Install R: <https://cran.r-project.org/mirrors.html> (version 4.1.1
    or greater)

2.  Install R Studio: <https://rstudio.com/products/rstudio/download/>
    (version 1.2.5001-3 or newer)

3.  Make sure the *devtools* package is installed, if not install it
    with:

``` r
install.packages("devtools")
```

4.  After that install the actual package:

``` r
devtools::install_github("michael-cw/susoparaviewer")
```

## Running the application interactively

You can either run the application interactively:

``` r
library(susoparaviewer)
susoparaviewer::runParaApp()
```

## Running the application on a Shiny Server

…or run the application on your own shiny server. In this case, create a
directory, i.e. susoparaviewer and put a file called *app.R* with the
following code into it:

``` r
library(susoparaviewer)
susoparaviewer::runParaAppServer()
```

#### Attention - Potential issue with ‘shinyalert’

There’s a known issue with the ‘shinyalert’ package from Dean Attali,
which may cause the application to fail during start-up on certain
Windows installations running the latest version of R. For more details,
please check this
[issue](https://github.com/daattali/shinyalert/issues/75).

This issue has been resolved in the development version of ‘shinyalert’,
but the fix is not yet available in the official CRAN release. If you
encounter this problem, please install the development version for
‘shinyalert’ using the following command:

``` r
devtools::install_github("daattali/shinyalert")
```

#### EXPERIMENTAL OPTIONAL FEATURE: GPSLogger tracking

The application also contains an optional feature, which allows you to
track enumerators during fieldwork. This may be useful for example when
you conduct a pre-survey listing of household units, where coverage of
area sampling units is important. To use this feature two prerequisites
are needed:

1.  The [GPSLogger application](https://gpslogger.app/), which is an
    open source, lightweight GPS logger.
2.  A PostgreSQL database with the logging data stored in the following
    *gpslogger* table:
    - lat double precision Latitude
    - long double precision Longitude
    - id text user id included in the url
    - aid text android id
    - time text time
    - acc double precision accuracy
    - bat double precision battery percentage

You can either set this up in your own way, or use the
[NiFI](https://nifi.apache.org/) based solution for which you can find
the template in the
[NIFI-GPSLogger-with-SSL](https://github.com/michael-cw/NIFI-GPSLogger-with-SSL)
repo. The latter also contains to bash scripts to set-up NIFI as well as
the required PostgreSQL database on Ubuntu 22.04. Nevertheless for both
cases, you should know what you do, and be in compliance with local
labor and privacy regulations.

However if you decide to use this feature, then you need to provide the
required PostgreSQL database connection details when starting the app.

You can either run the application interactively:

``` r
library(susoparaviewer)
susoparaviewer::runParaApp(useTrackingPass = [yourinapptrackingpass], 
                           trackServer = "10.10.10.10", 
                           trackServerPort = 5432, 
                           trackServerDB = "gpsloggerdb", 
                           trackServerUser = [yourPGuser], 
                           trackServerPass = [yourPGpassword], 
                           trackServerTable = "gpslogger")
```

With the *useTrackingPass* password you can restrict access inside the
app. For the server option, the command would then look like this:

``` r
library(susoparaviewer)
susoparaviewer::runParaAppServer(useTrackingPass = [yourinapptrackingpass], 
                                 trackServer = "10.10.10.10", 
                                 trackServerPort = 5432, 
                                 trackServerDB = "gpsloggerdb", 
                                 trackServerUser = [yourPGuser], 
                                 trackServerPass = [yourPGpassword], 
                                 trackServerTable = "gpslogger")
```

##### Automatically create configuration files for your team

In addition to monitoring fieldwork activities, the app also includes
the option to generate [GPSLogger configuration
files](https://gpslogger.app/#loadprofilesfromafileonyourdevice) either
for all enumerators on the Survey Solutions server or for selected
teams. For details see the corresponding vignette.

## Feature requests and bug reports

You can either use the standard GitHub approach by filing a bug
report/feature request
[here](https://github.com/michael-cw/SurveySolutionsAPI/issues) or you
use the Survey Solutions user forum
[here](https://forum.mysurvey.solutions/c/api/13).

Please continue to check for updates, as we are constantly working to
improve the application.

[^1]: Funding was received from the [Joint Development Data Center on
    Forced Displacement](https://www.jointdatacenter.org/) .

[^2]: Only available if geographic coordinates had been collected.

[^3]: Only available if geographic coordinates had been collected.

    </div>
