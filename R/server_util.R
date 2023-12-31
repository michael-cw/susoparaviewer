#' Shiny server helper functions
#'
#' @description helper functions for server:
#' - stat_mode statistical mode
#' - freestyler style function for report
#' - shpMapOSM/shpMapOSM_cont creates map for report
#'
#' @rdname internal
#' @noRd

stat_mode<-function(x) {
  # mode solution from: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
  ux<-unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

freestyler <- function(bodytext = "Lorem Ipsium") {
  ## style
  fp_qte <- officer::fp_text(color = "#546e7a", font.size = 12, bold = T)
  fp_qte_sty <- officer::fp_par(text.align = "left", padding.bottom = 2, keep_with_next = T)
  ## text
  qte <- ftext(bodytext, fp_qte)
  ## block list
  out <- block_list(
    fpar(qte,
         fp_p = fp_qte_sty,
         run_linebreak()
    )
  )
  return(out)
}

shpMapOSM<-function(shp = NULL, z_var = NULL) {
  ## Check for stratum, if not generate 1
  if (is.null(z_var)) {
    z_var<-"stratum_numeric"
    shp$stratum_numeric<-"1"
  }
  if(!is.null(shp)) {
    shp<-st_transform(shp, 4326)
    bb<-st_bbox(shp)
    ## make z_var discreet
    if(!is.character(shp[[z_var]])) {
      shp[,z_var]<-as.character(shp[[z_var]])
    }

    names(bb)<-c("left", "bottom", "right", "top")
    osmmap<-tryCatch(
      {get_map(bb, source = "osm", maptype = "roadmap", scale = 9)},
      error = function(e) {get_map(bb, source = "stamen", maptype = "terrain")})
    shapePlot_baseMap<-ggmap(osmmap)+
      geom_sf(data = shp, aes(fill = .data[[z_var]], color = .data[[z_var]]), inherit.aes = FALSE, alpha = 0.3)+
      scale_fill_discrete(name = z_var) +
      scale_color_discrete(guide = "none") +
      ggplot2::theme(legend.position = "bottom")

    return(shapePlot_baseMap)
  }
}

shpMapOSM_cont<-function(shp = NULL, z_var = NULL) {
  ## Check for stratum, if not generate 1
  if (is.null(z_var)) {
    z_var<-"Aggregate"
  }
  if(!is.null(shp)) {
    shp<-st_transform(shp, 4326)
    bb<-st_bbox(shp)
    ## make z_var discreet
    # if(!is.character(shp[[z_var]])) {
    #   shp[,z_var]<-as.character(shp[[z_var]])
    # }

    names(bb)<-c("left", "bottom", "right", "top")
    osmmap<-tryCatch(
      {get_map(bb, source = "osm", maptype = "roadmap", scale = 9)},
      error = function(e) {get_map(bb, source = "stamen", maptype = "terrain")})
    shapePlot_baseMap<-ggmap(osmmap)+
      geom_sf(data = shp, aes(fill = .data[[z_var]], color = .data[[z_var]]), inherit.aes = FALSE, alpha = 0.5)+
      ggplot2::scale_fill_viridis_c(name = z_var, option = "magma") +
      ggplot2::scale_color_viridis_c(guide = "none", option = "magma") +
      ggplot2::theme(legend.position = "bottom")

    return(shapePlot_baseMap)
  }
}

#' Modal with error
#'
#' @noRd
#' @keywords internal
#'

.runWithModalOnError <- function(func) {
  result <- tryCatch(
    {
      func
    },
    error = function(err) {
      # Display the error message in a Shiny modal
      shinyalert::shinyalert(
        inputId = "errormodal",
        title = HTML("<div align='center'>ERROR</div>"),
        text = HTML(paste("<div align='center'>An error occurred:<font color='red'>",
                          err$message,
                          "</font></div>")),
        type = "error",
        html = TRUE
      )

      return(NULL)
    }
  )

  return(result)
}

.shinyalertVarSelection<-function(varnames, inputId) {
  shinyalert::shinyalert(
    inputId = "gpsvarmodal",
    title = "Select GPS variable for mapping.",
    text = tagList(
      shiny::selectInput(
        inputId = inputId,
        label = "",
        choices = varnames
      )
    ),
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = TRUE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#0d47a1",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )

}
