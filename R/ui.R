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

  # #slide-out

  material_page(
    # shiny alert conditional on version
    if (utils::packageVersion("shinyalert") < 3) shinyalert::useShinyalert(),
    waiter::use_waiter(),
    startupModalUI("startupModal"),
    nav_bar_color = "blue darken-4",
    background_color = "white",
    title = "Survey Solutions Paradata Viewer",
    tags$br(),
    # css styles
    tags$script(HTML(
      "
      const dropdown = document.querySelector('.dropdown');
      const dropdownContent = document.querySelector('.dropdown-content');

      dropdown.addEventListener('click', () => {
      dropdownContent.style.display = dropdownContent.style.display === 'block' ? 'none' : 'block';
      });
      "
    )),
    tags$style("
      #slide-out.sidenav {
        width: 30vw;
      }
      #modal-fba285d0-e69b-072b-8c6f-33f90c89a607 {
        transform: scaleX(2) scaleY(2) !important;
      }
      .dropdown-content {
            -webkit-text-size-adjust: 100%;
            line-height: 1.5;
            font-weight: normal;
            color: rgba(0,0,0,0.87);
            font-size: 15px;
            box-sizing: inherit;
            box-shadow: 0 2px 2px 0 rgba(0,0,0,0.14),0 3px 1px -2px rgba(0,0,0,0.12),0 1px 5px 0 rgba(0,0,0,0.2);
            background-color: #fff;
            margin: 0;
            min-width: 100px;
            overflow-y: auto;
            position: absolute;
            z-index: 9999;
            padding-left: 0;
            list-style-type: none;
            display: none;
            max-height: 200px; /* updated max-height */
            width: 100%;
            left: 0;
            top: 0;
            transform-origin: 0 0;
            opacity: 1;
            transform: scaleY(1);
        }

      .shiny-notification {
             width: 250px !important;
             position:fixed;
             top: 65px;
             right: 10px;
             color: #FFFFFF;
             background-color: #0d47a1;
             }

      .shiny-notification-content-text {
        overflow-wrap: break-word !important;
      }

      /*change color and opacity for warning */
      .shiny-notification-warning {
             background-color:#ff0f0f;
             color: #FFFFFF;
             opacity: 0.7;
      }
      .btn-file {
              color: #FFFFFF;
              background-color: #0d47a1;
              width: 80%;
              border-color: #0d47a1;
              display: block;
              margin-left: auto;
              margin-right: auto;
      }
      .btn-file:hover {
              background: #0d6ca1;
      }
    "),
    material_side_nav(
      fixed = FALSE,
      image_source = "www/suso_wb.png",
      material_side_nav_tabs(
        side_nav_tabs = c(
          "Charts & Maps" = "charts",
          "Source Data (including download)" = "data",
          "GPS tracking" = "tracking"
        ),
        icons = c("insert_chart", "explore", "location_searching")
      ),
      br(), br(),
      br(),
      useShinyjs(),
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
                         btnlabel = "Download Questionnaire Report"
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
                         btnlabel = "Download Interviewer Report"
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
                  material_dropdown("mapData",
                                    "Select Data to display",
                                    c("",
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
                         btnlabel = "Download Map Report"
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
                width = 3
              ),
              material_column(
                width = 6,
                dwl_dataUI("para_download",
                           "Download Paradata",
                           style = "color: #FFFFFF;width: 100%;background-color: #0d47a1;border-color: #2e6da4")
              ),
              material_column(
                width = 3
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
            material_password_box("pass", "Provide Password for Tracking!", color = "#0d47a1"),
            shinyjs::hidden(
              div(id = "trackingdiv",
                  br(),
                  material_dropdown("team", "Select Team", c("No Data Loaded!")),
                  br(), br(), br(),
                  ####################### ADMIN PANEL TRACK  ################################################
                  ## ADMIN PANEL TRACK
                  material_modal(
                    modal_id = "adminTrack",
                    button_text = "GPSLogger Configuration",
                    button_icon = "satellite",
                    title = "Generate Configuration Files",
                    button_color = "blue darken-4",
                    # tags$h5("Admin Panel"),
                    # br(),
                    material_row(
                      material_column(
                        width = 4,
                        numericInput("acc",
                                     "Desired Precision (m, max. 1000)?",
                                     min = 5,
                                     max = 1000,
                                     step = 5,
                                     value = 15
                        )
                      ),
                      material_column(
                        width = 4,
                        numericInput("trackRefresh",
                                     "Check location every (in seconds):",
                                     value = 60,
                                     min = 30,
                                     max = 1000
                        )
                      ),

                      material_column(
                        width = 4,
                        numericInput("trackDistance",
                                     "Log location every (in meters):",
                                     value = 2,
                                     min = 1,
                                     max = 100
                        )
                      )
                    ),
                    material_row(
                      material_column(
                        width = 4,
                        textInput("trackingIP",
                                  "Please provide server address",
                                  placeholder = "Required Format: https://10.10.10.10"
                        )
                      ),
                      material_column(
                        width = 4,
                        numericInput("trackingPort",
                                     "Please provide server port",
                                     value = 8080
                        )
                      ),
                      material_column(
                        width = 4,
                        textInput("trackUser",
                                  "Please provide server username",
                                  value = "admin"
                        )
                      )
                    ),
                    material_row(
                      material_column(
                        width = 4,
                        passwordInput("trackPass",
                                      "Please provide server password"
                        )
                      ),
                      material_column(
                        width = 4,
                        material_switch("trackAll",
                                        off_label = "All Teams",
                                        on_label = "Single Team",
                                        initial_value = FALSE
                        )
                      ),
                      material_column(
                        width = 4,
                        conditionalPanel(
                          "input.trackAll==true",
                          material_dropdown("teamConfig",
                                            "Select Team",
                                            choices = setNames("No Data Loaded!", "No Data Loaded!")
                          )
                        )
                      )
                    ),
                    material_row(
                      material_column(width = 4),
                      material_column(
                        width = 4, br(),
                        # downloadButton("downloadGPSLoggerConfig",
                        #                label = "Download GPSlogger\n Configuration Files",
                        #                style = "color: #fff; background-color: #0d47a1; border-color: #2e6da4"
                        # )
                        actionButton(("downloadGPSLoggerConfig"),
                                     label = "Download GPSlogger\n Configuration Files",
                                     icon("download"), width = "100%",
                                     style="color: #fff; background-color: #0d47a1; border-color: #2e6da4"
                                     ),
                        downloadButton(("downloadGPSLoggerConfigDwl"), "Not visible", style="visibility: hidden;")
                      ),
                      material_column(width = 4)
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
            leafletOutput("trackMap", height = "80vh")
          )
        )
      )
    )
    ################################## FIN #################################
  )
}
