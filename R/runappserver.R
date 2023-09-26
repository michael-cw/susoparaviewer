#' Start the Survey Solutions Paradata Viewer Application on a Shiny Server
#'
#' @description A wrappter function to start the application. Please make sure you have read the
#' documentation on how to use the app.
#'
#' @details
#' This application is part of the large set of tools, to facilitate survey implementation with
#' [Survey Solutions](https://docs.mysurvey.solutions/). Survey Solutions Paradata is data about the actual
#' data collection process, however it comes with some challenges in processing. This application seeks to minimize these challenges,
#' by transforming the data, and subsequently producing plots and reports from it.
#'
#'
#'
#' @inherit shiny::runApp
#' @param mapwidget.option Selection of map at start-up, mapdeck or leaflet, defaults to leaflet if NULL
#' @param useTrackingPass Passcode to access tracking section, if NULL, tracking is disabled
#' @param trackServer PostgreSQL tracking DB URL
#' @param trackServerPort PostgreSQL tracking DB port
#' @param trackServerDB PostgreSQL tracking DB name
#' @param trackServerUser PostgreSQL tracking DB user
#' @param trackServerPass PostgreSQL tracking DB pasword
#' @param trackServerTable PostgreSQL tracking DB table with tracking data
#'
#'
#' @export
runParaAppServer <- function(launch.browser = TRUE,
                             mapwidget.option = c("leaflet", "mapdeck"),
                             useTrackingPass = NULL,
                             trackServer = NULL,
                             trackServerPort = 5432,
                             trackServerDB = NULL,
                             trackServerUser = NULL,
                             trackServerPass = NULL,
                             trackServerTable = NULL) {
  shiny::addResourcePath("www", system.file("www", package = "susoparaviewer"))
  shiny::addResourcePath("rmdfiles", system.file("rmdfiles", package = "susoparaviewer"))

  # option check
  mapwidget.option <- match.arg(mapwidget.option)

  # get original options
  original_options <- list(
    #shiny.maxRequestSize = getOption("shiny.maxRequestSize"),
    # You might want to store your original spinner.color.background if it's set somewhere in your code
    #spinner.color.background = getOption("spinner.color.background"),
    mapwidget.option = getOption("mapwidget.option"),
    useTrackingPass = getOption("useTrackingPass"),
    trackServer = getOption("trackServer"),
    trackServerPort = getOption("trackServerPort"),
    trackServerDB = getOption("trackServerDB"),
    trackServerUser = getOption("trackServerUser"),
    trackServerPass = getOption("trackServerPass"),
    trackServerTable = getOption("trackServerTable")
  )
  # change options and revert on stop
  changeoptions <- function() {
    options(
      # Temporary change of environment options
      mapwidget.option = mapwidget.option,
      shiny.maxRequestSize = 500000 * 1024^2,
      useTrackingPass = useTrackingPass,
      trackServer = trackServer,
      trackServerPort = trackServerPort,
      trackServerDB = trackServerDB,
      trackServerUser = trackServerUser,
      trackServerPass = trackServerPass,
      trackServerTable = trackServerTable
    )
    #shiny::shinyOptions(shiny.maxRequestSize = 500000 * 1024^2)

    # revert to original state at the end
    shiny::onStop(function() {
      if(!is.null(original_options)){
        options(original_options)
      }
    })
  }
  shiny::shinyApp(ui = susoparaviewer:::main_ui, server = susoparaviewer:::main_server, onStart = changeoptions)
}
