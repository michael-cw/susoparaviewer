##############################
## Reports for DWL with OFFICER
##############################


###############################################################################
## Function for SPATIAL OBJECTS
###############################################################################
## 1. SF with OSM
shpMapOSM <- function(shp = NULL, z_var = NULL, bb = NULL) {
  ## Check for stratum, if not generate 1
  if (is.null(z_var)) {
    z_var <- "stratum_numeric"
    shp$stratum_numeric <- "1"
  }
  if (!is.null(shp)) {
    if (is.null(bb)) {
      shp <- st_transform(shp, 4326)
      bb <- st_bbox(shp)
      # mapmin<-0.0009
      # bb["xmin"]<-bb["xmin"]*mapmin
      # bb["ymin"]<-bb["ymin"]*mapmin
      # mapmax<-1.0001
      # bb["xmax"]<-bb["xmax"]*mapmax
      # bb["ymax"]<-bb["ymax"]*mapmax
    } else {
      bb <- st_transform(bb, 4326)
      bb <- st_bbox(shp)
      # mapmin<-0.0009
      # bb["xmin"]<-bb["xmin"]*mapmin
      # bb["ymin"]<-bb["ymin"]*mapmin
      # mapmax<-1.0001
      # bb["xmax"]<-bb["xmax"]*mapmax
      # bb["ymax"]<-bb["ymax"]*mapmax
    }


    ## make z_var discreet
    # if(!is.character(shp[[z_var]])) {
    #   shp[,z_var]<-as.character(shp[[z_var]])
    # }
    ###########################################
    ## 1. Residential buildings only
    # map_build<- opq(bbox = bb) %>%
    #   add_osm_feature(key="building") %>%
    #   osmdata_sf()
    ## 2. Boundaries
    # map_boundary<- opq(bbox = bb) %>%
    #   add_osm_feature(key="boundary",
    #                   value = "administrative") %>%
    #   osmdata_sf()
    # map_road<-opq(bbox = bb) %>%
    #   add_osm_feature(key="highway") %>%
    #   osmdata_sf()
    #
    # map_landuse<-opq(bbox = bb) %>%
    #   add_osm_feature(key="landuse") %>%
    #   osmdata_sf()
    ###########################################
    names(bb) <- c("left", "bottom", "right", "top")
    osmmap <- get_stamenmap(bb, maptype = "toner", zoom = 13)
    shapePlot_baseMap <- ggmap(osmmap, extent = "device") +
      geom_sf(
        data = shp, aes(color = .data[[z_var]], fill = .data[[z_var]]),
        inherit.aes = FALSE, alpha = 0.6
      ) +
      scale_fill_continuous(name = z_var, type = "viridis") +
      scale_color_continuous(guide = FALSE, type = "viridis")
    # theme(legend.position = "none")

    return(shapePlot_baseMap)
  }
}

## 2. SF with OSM
rasMapOSM <- function(ras = NULL, z_var = "MOS") {
  if (!is.null(ras)) {
    ## 1. Transform to stars proxy
    ## i. get dimension from RASTER
    xdim <- res(ras)[1]
    ydim <- res(ras)[2]
    ## ii. save & reload as stars proxy
    tmpFile <- tempfile("raster_proxy", fileext = ".tif")
    unlink(tmpFile)
    writeRaster(ras, tmpFile)
    rm(ras)
    ## ii. read Proxy
    ras <- read_stars(tmpFile, proxy = T)
    ## ii. use st_wrap (aggreagate and rasterio did not work!)
    repras <- st_as_stars(st_bbox(ras), dx = xdim * 5, dy = ydim * 5)
    ras <- st_warp(ras, repras)
    stars::st_as_stars(ras)
    ## for plotting empty cell with zero
    ras[is.na(ras)] <- 0

    # ras<-st_transform(ras, 4326)
    bb <- st_bbox(ras)
    names(bb) <- c("left", "bottom", "right", "top")
    osmmap <- get_map(bb, source = "osm", maptype = "roadmap")
    shapePlot_baseMap <- ggmap(osmmap) +
      # geom_sf(data = map_road$osm_lines, inherit.aes = FALSE, color="red", size=0.5)+
      # geom_sf(data = map_landuse$osm_points, inherit.aes = FALSE, color="blue", size=0.5)+
      stars::geom_stars(data = ras, alpha = 0.5) +
      coord_equal() +
      scale_fill_viridis_c(name = z_var) +
      theme(legend.position = "bottom")

    return(shapePlot_baseMap)
  }
}
########################################################################################################
####                      CSS & HTML for shiny_dwl_report
########################################################################################################
## scroll table

styleActButtonActivate <- c("color: #FFFFFF; background-color: #546e7a;
                  border-color: #546e7a; margin:0% 0% 0% 0%;")
styleDwlButton <- c("color: #FFFFFF;  width: 180px;background-color: #FFFFFF;
                  border-color: #FFFFFF;
                  margin:0 20% 0 20%;")

invisibleButton <- c("color: #FFFFFF; background-color: #FFFFFF;
                  border-color: #FFFFFF; margin:0% 0% 0% 0%;height:2px;")


#####################################################################################################
##            UI COMPONENT
#####################################################################################################
dwl_reportUI <- function(id, label = "Download the report") {
  ns <- NS(id)
  #############################
  ## output format is dynamic
  ##    - leafletouput for small
  ##    - mapdeck for large
  tagList(
    shinyjs::useShinyjs(),
    material_row(
      # add_busy_bar(color = "#68cbf8", timeout = 60000, height = "10px"),
      material_column(width = 3),
      material_column(
        width = 6,
        actionButton(ns("generateReportInt"),
          label = label,
          icon("chart-bar"), width = "100%",
          style = styleActButtonActivate
        )
      ),
      material_column(width = 3)
    ),
    material_row(
      ## Button is INVISIBLE, activated by shinyjs::click
      material_column(width = 3),
      material_column(
        width = 6,
        downloadButton(ns("dwl_report"), "Not visible", style = invisibleButton)
      ),
      material_column(width = 3)
    )
  )
  #################### FIN UI####################################################
}
#####################################################################################################
##            SERVER COMPONENT
#####################################################################################################
## Generates WORD reports, for the following LIST of input elements
##    - doc_titel = character(1)
##    - sec_title = character(number of sections)
##    - sec_para = list(para1 =
##                      para2 = ....
##                      number of para)
##    - sec_table = list(para1 = ...)
##    - sec_figure = list(para1 = ...)
###################################################################
## ATTENTION: LIST MUST BE SYMMETRIC,
###################################################################
dwl_reportSRV <- function(input, output, session, rname = NULL,
                          content = NULL, creator = "Survey Solutions Paradata Viewer", created = Sys.time()) {
  ## Initate Report Creation
  observeEvent(input$generateReportInt, {
    req(content())
    rep_cont <- content()


    #####################
    ## 1. Get Content
    ### 2.1 Generate File
    withProgress(message = "Preparing Word Document", value = 0, {
      doc.full <- read_docx(file.path(system.file("rmdfiles", package = "susoparaviewer"), "FINAL_report_for_download.docx")) %>%
        set_doc_properties(
          title = "Survey Solutions Quality Report Tools",
          creator = creator,
          created = created
        )

      ##############################
      ## 2.1 DOC TITLE & DATE
      doc.full <- doc.full %>%
        body_add(rep_cont$doc_title) %>%
        body_add_break()
      ## 2.3. Add Section Para and Tables
      ##    i. loop over section
      ##        ii. loop over para
      incProgress(0.2)
      for (sec_para in names(rep_cont$sec_para)) {
        incProgress(0.2)
        p <- rep_cont$sec_para[[sec_para]]
        t <- rep_cont$sec_table[[sec_para]]
        i <- rep_cont$sec_graph[[sec_para]]
        sectitle <- rep_cont$sec_title[[sec_para]]
        doc.full <- doc.full %>%
          body_add_par(sectitle, style = "heading 2") %>%
          body_add_par(NULL, style = "Normal")
        for (para in names(p)) {
          pp <- p[[para]]
          tt <- t[[para]]
          ii <- i[[para]]
          ## add para
          if (!is.null(pp)) {
            doc.full <- doc.full %>%
              body_add(pp, style = "Normal") %>%
              body_add_par(NULL, style = "Normal")
          }
          ## add table
          if (!is.null(tt)) {
            doc.full <- doc.full %>%
              body_add_table(tt,
                alignment = "c",
                style = "Grid Table 6 Colorful",
                header = T
              ) %>%
              body_add_par(NULL, style = "Normal") %>%
              body_add_par(NULL, style = "Normal") %>%
              body_add_break()
          }
          ## add graph
          if (!is.null(ii)) {
            doc.full <- doc.full %>%
              body_add_gg(ii, style = "Figure") %>%
              body_add_par(NULL, style = "Normal") %>%
              body_add_par(NULL, style = "Normal") %>%
              body_add_break()
          }
        }
        doc.full <- doc.full %>%
          body_add_break()
      }
      ## 2.2. Switch working directory for report
      wdOld <- getwd()
      setwd(tempdir())
      on.exit(setwd(wdOld))

      ## 2.3. Tempfile
      doc.full %>%
        print(target = "report_for_download_v1.docx")
    })
    ## 2.4. Click DWL button
    shinyjs::click("dwl_report")
  })
  ## Download Report
  output$dwl_report <- downloadHandler(
    filename = function() {
      paste("RL-", rname, "-", stringr::str_remove_all(Sys.time(), "[:space:]|[:punct:]"), ".docx", sep = "")
    },
    content = function(file) {
      wdOld <- getwd()
      setwd(tempdir())
      on.exit(setwd(wdOld))
      file.copy("report_for_download_v1.docx", file)
    }, contentType = NULL
  )

  #################### FIN SERVER####################################################
}
