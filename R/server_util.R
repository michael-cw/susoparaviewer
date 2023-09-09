#' Shiny server helper functions
#'
#' @description helper functions for server:
#' - stat_mode statistical mode
#'
#' @rdname internal
#' @noRd

stat_mode<-function(x) {
  # mode solution from: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
  ux<-unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
