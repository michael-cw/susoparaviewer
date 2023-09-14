#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom broom tidy
#' @importFrom data.table as.data.table as.IDate as.ITime copy data.table first fread fwrite getDTthreads is.data.table rbindlist setkeyv setnames setorderv shift tstrsplit
#' @importFrom doFuture registerDoFuture
#' @importFrom dplyr n_distinct select
#' @importFrom DT datatable dataTableOutput formatStyle renderDataTable
#' @importFrom foreach foreach `%dopar%`
#' @importFrom future plan sequential
#' @importFrom ggmap get_map get_stamenmap ggmap
#' @importFrom ggplot2 aes alpha coord_equal element_blank element_line element_rect element_text geom_sf scale_color_continuous scale_color_discrete scale_fill_continuous scale_fill_discrete scale_fill_viridis_c theme theme_bw unit
# #' @importFrom graphics layout
#' @importFrom grDevices colorRampPalette
#' @importFrom httr authenticate content GET POST timeout write_disk
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom leaflet addLegend addPolygons addPolylines addProviderTiles addTiles clearShapes colorFactor colorNumeric fitBounds leaflet leafletOutput leafletProxy popupOptions providers providerTileOptions renderLeaflet setView
#' @importFrom lubridate hour isoweek mday month wday as_datetime hms
#' @importFrom mapdeck add_arc add_grid add_path add_polygon add_scatterplot clear_arc clear_grid clear_polygon clear_scatterplot mapdeck mapdeck_style mapdeck_update mapdeck_view mapdeckOutput renderMapdeck
#' @importFrom methods as
#' @importFrom officer block_list body_add body_add_break body_add_gg body_add_par body_add_table external_img fp_border fp_par fp_text fpar ftext read_docx run_linebreak set_doc_properties read_pptx add_slide ph_with ph_location_label ph_location_type
#' @importFrom plotly add_annotations add_lines add_trace event_data event_register layout plot_ly plotlyOutput renderPlotly select
#' @importFrom raster as.data.frame as.factor as.list coordinates getData levels mean merge modal nrow print res subset unique writeRaster
#' @importFrom RColorBrewer brewer.pal
#' @importFrom readr write_file
#' @importFrom sf st_area st_as_sf st_as_sfc st_bbox st_centroid st_coordinates st_crop st_crs st_is_empty st_is_longlat st_join st_set_crs st_set_geometry st_transform st_within
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom shinyalert shinyalert useShinyalert
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs click hidden toggleElement useShinyjs
#' @importFrom shinymaterial material_button material_card material_column material_date_picker material_dropdown material_file_input material_modal material_number_box material_page material_password_box material_radio_button material_row material_side_nav material_side_nav_tab_content material_side_nav_tabs material_spinner_hide material_spinner_show material_switch material_tab_content material_tabs update_material_date_picker update_material_dropdown
#' @importFrom sp coordinates merge SpatialLinesLengths
#' @importFrom stars geom_stars read_stars st_as_stars st_rasterize st_warp
#' @importFrom stats fivenum median setNames start var
#' @importFrom stringr str_count str_remove_all
#' @importFrom SurveySolutionsAPI suso_clear_keys suso_getQuestDetails suso_PwCheck suso_set_key
#' @importFrom utils capture.output download.file str unzip zip
#' @importFrom waiter spin_fading_circles
#'
## usethis namespace: end
NULL

if(getRversion() >= "3.3.0")  {
  utils::globalVariables(c(
    "i", "NAME",
    "long", "lat", "OBJECTID", "NAME_2", "Removals",
    "var_resp", "V1", "V2", "V3", "dateTime", "wDAY", "mDAY", "MONTH", "WEEK", "role", "action", "responsible",
    "action", "rid", "breaks", "resp_time",
    "QuestionnaireIdVersion",
    "QuestionnaireId",
    "Version",
    "QuestionText",
    "PublicKey",
    ".",
    "interview__id",
    "counterMedian",
    "Av_ResponseTime",
    "durationNOBREAK",
    "key",
    "Total_Responsetime",
    "LastEntryDate",
    "time",
    "tz",
    "m_resp_time_varTRIM",
    "m_diff_dev",
    "Mean_Deviation", "Invalids", "key2", "surveyDay", "duration", "startHour", "mean_duration", "mean_durationNOBREAK",
    "mean_RespTime", "tot", "durationNOBREAK_sec", "response", "response1", "response2", "count", "counter",
    "m_resp_time_var", "duration_sec", ".data", "AverageTimePerInterview", "NumberOfInterviews",
    "VariableName", "type"
  ))





}
