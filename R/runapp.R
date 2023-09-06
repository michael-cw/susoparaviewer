#' Start the Survey Solutions Questionnaire Manual Application
#'
#' @description A wrappter function to start the application. Please make sure you have read the
#' documentation on how to use the app.
#'
#' @details
#' This application is part of the large set of tools, to facilitate survey implementation with
#' [Survey Solutions](https://docs.mysurvey.solutions/). Enumeration Areas, sampled for example
#' with the JDC-Survey Solutions Spatial Sampling application, can be used to enumerate all buildings
#' visible within the boundaries on a google map. The enumerate buildings can then be used for the
#' second stage sampling frame and to draw the survey units within the cluster from it.
#'
#'
#'
#' @inherit shiny::runApp
#' @param mapwidget.option Selection of map at start-up, mapdeck or leaflet, defaults to leaflet if NULL
#'
#'
#' @export
runParaApp <- function(launch.browser = TRUE,
                       mapwidget.option = c("leaflet", "mapdeck")) {
  shiny::addResourcePath("www", system.file("www", package = "susoparaviewer"))
  shiny::addResourcePath("rmdfiles", system.file("rmdfiles", package = "susoparaviewer"))

  # option check
  mapwidget.option <- match.arg(mapwidget.option)

  # get original options
  original_options <- list(
    shiny.maxRequestSize = getOption("shiny.maxRequestSize"),
    # You might want to store your original spinner.color.background if it's set somewhere in your code
    #spinner.color.background = getOption("spinner.color.background"),
    mapwidget.option = getOption("mapwidget.option")
  )
  # change options and revert on stop
  changeoptions <- function() {
    options(
      # Temporary change of environment options
      mapwidget.option = mapwidget.option,
      shiny.maxRequestSize = 500000 * 1024^2
    )
    #shiny::shinyOptions(shiny.maxRequestSize = 500000 * 1024^2)

    # revert to original state at the end
    shiny::onStop(function() {
        options(original_options)
    })
  }
  appObj<-shiny::shinyApp(ui = main_ui, server = main_server, onStart = changeoptions)
  shiny::runApp(appObj, launch.browser = launch.browser, quiet = T)
}
