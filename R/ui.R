#' Ui function for Spatial Sampling App
#'
#' @noRd
#' @keywords internal
#'

main_ui <- function(request) {
  styleActButtonActivate <- c("color: #FFFFFF; background-color: #0d47a1;
                  border-color: #0d47a1; margin:0% 20% 0% 0%;")

  styleActButtonActivate <- c("color: #FFFFFF; background-color: #0d47a1;
                  border-color: #0d47a1; margin:0% 20% 0% 0%;")
  styleDwlButton <- c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")

  material_page(
    # shiny alert conditional on version
    if (utils::packageVersion("shinyalert") < 3) shinyalert::useShinyalert(),
    startupModalUI("startupModal"),
    nav_bar_color = "blue-grey darken-1",
    background_color = "white",
    title = "Survey Solutions Paradata Viewer",
    tags$br(),
    material_side_nav(
      fixed = FALSE,
      image_source = "www/suso_wb.png",
      material_side_nav_tabs(
        side_nav_tabs = c(
          "Charts & Maps" = "charts",
          "Source Data (including download" = "data",
          "GPS tracking" = "tracking"
        ),
        icons = c("insert_chart", "explore", "location_searching")
      ),
      br(), br(),
      br(),
      useShinyjs(),
      useShinyalert(),
      material_row(
        material_column(
          width = 12,
          tags$h6("Time Period")
        )
      ),
      material_row(
        material_column(
          width = 6, material_date_picker("dateFrom", label = "From")
        ),
        material_column(
          width = 6, material_date_picker("dateTo", label = "To")
        )
      )
    ),
    material_side_nav_tab_content(
      side_nav_tab_id = "charts",
      material_row(
        material_column(
          width = 12,
          material_tabs(
            tabs = c(
              "Questionnaire Level" = "respTime",
              "Interview Level" = "interTime",
              "Maps" = "map"
            ),
            color = "red"
          ),
          ####################### Questionnaire ############################
          material_tab_content(
            tab_id = "respTime",
            material_row(
              material_column(
                width = 4,
                material_card(
                  title = "",
                  depth = 4,
                  plotlyOutput("timePlot"), br(), br(), br()
                  # material_slider("timeSlide", "Survey Week", 0, 50, initial_value = 0)
                )
              ),
              material_column(
                width = 4,
                material_card(
                  title = "",
                  depth = 4,
                  plotlyOutput("qTotPlot", width = "100%"),
                  material_radio_button("shortLong", "Shortest / longest (maximum 30 interviews)",
                    c("Shorter", "Longer"),
                    color = "#0d47a1"
                  )
                )
              ),
              material_column(
                width = 4,
                material_card(
                  title = "",
                  depth = 4,
                  plotlyOutput("invalidQuest", width = "100%"),
                  material_radio_button("invalHighLow", "Most/Least number of invalids (max. 30 interviews)",
                    c("Highest", "Lowest"),
                    color = "#0d47a1"
                  )
                )
              )
            ),
            material_row(
              material_column(width = 1),
              material_column(
                width = 10,
                material_card(
                  title = "",
                  depth = 4,
                  material_row(
                    material_column(
                      width = 4,
                      tags$h4("Questionnaire/Question details"),
                      plotlyOutput("timePlotPlot", height = "100px")
                    ),
                    material_column(
                      width = 8,
                      DT::dataTableOutput("timePlotTab", width = "50px"),
                      helpText("If you click the KEY, the interview will open in a new window (IMPORTANT: You must be logged in
                               to the Survey Solutions server and have the required authorization to perform this action)")
                    )
                  )
                )
              ),
              material_column(width = 1)
            ),
            dwl_reportUI("dwl_q_report",
              label = "Download the full report"
            )
          ),
          ############# Interviewer ###############################################
          material_tab_content(
            tab_id = "interTime",
            material_row(
              material_column(
                width = 4,
                material_card(
                  title = "",
                  depth = 4,
                  plotlyOutput("respPlot"),
                  material_radio_button("respfastSlow", "Fastest / Slowest (maximum 30 interviewers)",
                    c("Fast" = "Fast1", "Slow" = "Slow1"),
                    color = "#0d47a1"
                  )
                )
              ),
              material_column(
                width = 4,
                material_card(
                  title = "",
                  depth = 4,
                  plotlyOutput("DevPlot"),
                  material_radio_button("devfastSlow", "Fastest / Slowest (maximum 30 interviewers)",
                    c("Fast" = "Fast2", "Slow" = "Slow2"),
                    color = "#0d47a1"
                  )
                )
              ),
              material_column(
                width = 4,
                material_card(
                  title = "",
                  depth = 4,
                  plotlyOutput("answRemInt"),
                  material_radio_button("answRemHighLow", "Fewest / least number of response eliminations (maximum 30 interviewers)",
                    c("Most" = "Most3", "Least" = "Least3"),
                    color = "#0d47a1"
                  )
                )
              )
            ),
            material_row(
              material_column(width = 1),
              material_column(
                width = 10,
                material_card(
                  title = "",
                  depth = 4,
                  material_row(
                    material_column(
                      width = 4,
                      tags$h4("Interviewer Details")
                    ),
                    material_column(
                      width = 8,
                      DT::dataTableOutput("intPlotTab")
                    )
                  )
                )
              ),
              material_column(width = 1)
            ),
            dwl_reportUI("dwl_int_report",
              label = "Download the full report"
            )
          ),
          ############################ MAP #################################
          material_tab_content(
            tab_id = "map",
            material_row(
              material_column(
                width = 2,
                material_card(
                  title = "Display Settings",
                  depth = 4,
                  material_radio_button("mapData", "Select Data to display",
                    c(
                      "Time" = "Time",
                      "Removals" = "Removals",
                      "Invalids" = "Invalids"
                    ),
                    color = "#0d47a1"
                  ),
                  material_dropdown("mapSource",
                    "Which Input Map?",
                    c("GADM", "Landsat", "Own Map"),
                    selected = "GADM",
                    color = "#0d47a1"
                  )
                ),
                material_card(
                  title = "Area Details",
                  depth = 4,
                  DT::dataTableOutput("mapTab")
                )
              ),
              material_column(
                width = 10,
                material_card(
                  title = "",
                  depth = 4,
                  # leafletOutput("admMap", height = "650")
                  shiny::uiOutput("MAP_UI")
                )
              )
            ),
            material_row(
              material_column(
                width = 2
              ),
              material_column(
                width = 10
              )
            ),
            dwl_reportUI("dwl_m_report",
              label = "Download the full report"
            )
          )
        )
      )
    ),
    ################### DATA LOAD         #######################
    material_side_nav_tab_content(
      side_nav_tab_id = "data",
      material_row(
        material_column(
          width = 3,
          material_card(
            title = "",
            depth = 4,
            material_radio_button("dataLoad", "How do you want to load the data?",
              c("File", "Server", "LocalFile"), "File",
              color = "#0d47a1"
            ),
            conditionalPanel(
              "input.dataLoad=='File'",
              # material_file_input("file1",
              #   "Upload ORIGINAL ZIP FILE with
              #                             Paradata from your Survey Solutions Server",
              #   color = "#0d47a1"
              # )
              zipFileInput_ui(id = "file1",
                              label = "Upload ORIGINAL ZIP FILE with Paradata from your Survey Solutions Server",
                              accept = "application/zip")
            ),
            #################### SERVER SETTINGS     ##########################
            conditionalPanel(
              "input.dataLoad=='Server'",
              br(),
              material_modal(
                modal_id = "serverSettings",
                button_text = "Survey Solutions Server Settings",
                button_color = "blue darken-4",
                button_icon = "perm_data_setting",
                title = "Survey Solutions Server Settings",
                material_row(
                  material_column(
                    width = 6,
                    textInput("suso.server", "Provide SuSo Server",
                              placeholder = "Survey Solutions Server"),br(),br()),

                  material_column(
                    width = 6,
                    textInput("suso.user", "Provide SuSo API user",
                              placeholder = "API User"),br(),br())
                ),
                material_row(
                  material_column(
                    width = 6,
                    passwordInput("suso.pass", "Provide SuSo password",
                                  placeholder = "API Password"),br(),br()),
                  material_column(
                    width = 6,
                    textInput("suso.workspace", "Provide SuSo workspace",
                              value = "primary"),br(),br(),
                  )
                ),
                material_row(
                  material_column(
                    width = 6,
                    br(), br()
                  ),
                  material_column(
                    width = 6,
                    numericInput("suso.refresh",
                      "Refresh Interval (Minutes, max. 1440/24h)",
                      value = 60,
                      min = 30,
                      max = 1440,
                      step = 30
                    ), br(), br()
                  )
                ),
                material_row(
                  material_column(
                    width = 6,
                    material_button("suso.save",
                      "Save Admin Settings",
                      color = "blue darken-4",
                      icon = "save"
                    )
                  ),
                  material_column(
                    width = 6,
                    material_button("suso.refresh.now",
                      "Refresh Now",
                      color = "blue darken-4",
                      icon = "cloud_download"
                    )
                  )
                ),
                material_row(
                  material_column(
                    width = 6
                  ),
                  material_column(
                    width = 6
                  )
                )
              ),
              br(), br()
            ),
            br(), br(),
            br(),

            ######################### Select Questionnaire/File ############
            conditionalPanel(
              "input.dataLoad=='Server'",
              material_dropdown(
                input_id = "susoQuestionnaire",
                label = "Please select the Questionnaire",
                choices = c("Specify Server Settings First"),
                multiple = F,
                color = "#0d47a1"
              )
            ),
            material_dropdown("file_select_view",
              "Please select the paradata file",
              choices = c("(Up)Load Data First!"),
              multiple = F,
              color = "#0d47a1"
            ),
            helpText("ATTENTION: Data viewer does not show all data due to space reason.
                               The download file contains all."),
            br(), br(),
            br(),
            material_row(
              material_column(
                width = 6,
                actionButton("downloadData_ini",
                  label = "Create DWL File", icon = icon("fa-download"),
                  style = "color: #fff; background-color: #0d47a1; border-color: #2e6da4"
                )
              ),
              #######################    File info         #######################

              material_column(
                width = 6,
                shinyjs::hidden(
                  downloadButton("downloadData",
                    label = "Download", icon = icon("fa-download"),
                    style = "color: #fff; background-color: #0d47a1; border-color: #2e6da4"
                  )
                )
              )
            )
          )
        ),
        material_column(
          width = 9,
          material_card(
            title = "Data Viewer",
            depth = 4,
            DT::dataTableOutput("viewData")
          )
        )
      )
    ),
    material_side_nav_tab_content(
      side_nav_tab_id = "tracking",
      material_row(
        material_column(
          width = 3,
          material_card(
            title = "",
            depth = 4,
            conditionalPanel(
              "input.pass == 'Tracker1234'|input.pass == 'RomaniaLFS2018'",
              material_switch("loadtrack", "Load Interviewer Tracks",
                off_label = "No", on_label = "Yes",
                initial_value = FALSE
              ),
              br(), br(),
              br(),
              material_dropdown("team", "Select Team", c("No Data Loaded!"))
            ),
            material_password_box("pass", "Provide Password for Tracking!", color = "#0d47a1"),
            br(), br(), br(),
            ####################### ADMIN PANEL TRACK  ################################################
            ## ADMIN PANEL TRACK
            conditionalPanel(
              "input.pass == 'Tracker1234'|input.pass == 'RomaniaLFS2018'",
              material_modal(
                modal_id = "adminTrack",
                button_text = "Admin Settings",
                button_icon = "satellite",
                title = "Admin Settings Tracking",
                button_color = "blue darken-4",
                # tags$h5("Admin Panel"),
                # br(),
                material_row(
                  material_column(
                    width = 6,
                    numericInput("acc",
                      "Desired Precision (m, max. 1000)?",
                      min = 5,
                      max = 1000,
                      step = 5,
                      value = 15
                    )
                  ),
                  material_column(
                    width = 6,
                    numericInput("trackRefresh",
                      "Reload tracking data every (in seconds):",
                      value = 60,
                      min = 30,
                      max = 7200
                    )
                  )
                ),
                material_row(
                  material_column(
                    width = 6,
                    material_switch("trackMode",
                      initial_value = TRUE,
                      "Which tracking Software?",
                      on_label = "Owntracks",
                      off_label = "GPSlogger"
                    )
                  ),
                  material_column(
                    width = 6,
                    textInput("trackingIP",
                      "Please provide server address and port
                                                               (i.e. IP:PORT)",
                      value = "34.224.75.201:1883"
                    )
                  )
                ),
                conditionalPanel(
                  "input.trackMode==true",
                  material_row(
                    material_column(
                      width = 3
                    ),
                    material_column(
                      width = 6,
                      material_dropdown("teamTrack", "Select Team", c("No Data Loaded!")),
                      helpText("For tracking with OWNTRACKS each user receives a within team unique id. This needs to be
                                                      done for all teams which should be tracked.")
                    ),
                    material_column(
                      width = 3
                    )
                  ),
                  material_row(
                    material_column(
                      width = 6,
                      material_button(
                        input_id = "genTeamTrack",
                        label = "Generate Teams",
                        color = "blue darken-4",
                        icon = "create"
                      )
                    ),
                    material_column(
                      width = 6, br(),
                      downloadButton("downloadOwnTrConfig",
                        label = "Download Owntracks\n Configuration Files",
                        style = "color: #fff; background-color: #0d47a1; border-color: #2e6da4"
                      )
                    )
                  )
                ),
                conditionalPanel(
                  "input.trackMode==false",
                  material_row(
                    material_column(
                      width = 3
                    ),
                    material_column(
                      width = 6,
                      material_number_box("gpsLoggUser", "How many GPSlogger configuration files are required?",
                        min_value = 0, max_value = 999, initial_value = 1, color = "#0d47a1"
                      ),
                      helpText("For tracking with GPSlogger, it requires only the total number of interviewers which will be tracked, as everybody receives the
                                                                       same configuration file. Identification is done by the device ID.")
                    ),
                    material_column(
                      width = 3
                    )
                  ),
                  material_row(
                    material_column(width = 6),
                    material_column(
                      width = 6, br(),
                      downloadButton("downloadGPSLoggerConfig",
                        label = "Download GPSlogger\n Configuration Files",
                        style = "color: #fff; background-color: #0d47a1; border-color: #2e6da4"
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        material_column(
          width = 9,
          material_card(
            title = "",
            depth = 4,
            leafletOutput("trackMap", height = "700px")
          )
        )
      )
    )
    ################################## FIN #################################
  )
}
