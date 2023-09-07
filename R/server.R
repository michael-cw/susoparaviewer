# ` Shiny server
#'
#'
#'
#' @keywords internal
#' @noRd

# Sys.setenv(R_ZIPCMD="usr/bin/zip")
# options(java.parameters = "-Xmx2g") # increase heap size to 1gb
# options(shiny.maxRequestSize = 15000*1024^2)

main_server <- function(input, output, session) {
  #shiny::shinyOptions(shiny.maxRequestSize = 500000 * 1024^2)
  # options(shiny.maxRequestSize = 500000 * 1024^2)
  #################################
  ##  Table formats & styles
  ## 1. General
  smTab<-list(dom="t")

  ##  2. Info table (no selection, first column is Names)
  infoTable<-.%>% DT::formatStyle(1,  color = '#FFFFFF',
                                  backgroundColor = '#0d47a1')


  ##  3. View table (no selcetion, all columns the same)
  viewTable<-.%>% DT::formatStyle(1,  color = '#FFFFFF',
                                  backgroundColor = '#0d47a1',
                                  fontWeight = 'bold')



  para_data_coll <- reactiveValues()

  ## START UP MODAL FOR MAPKEY AND USER
  startupkeyusr <- startupModalSRV("startupModal", useronly = ifelse(getOption("mapwidget.option") == "mapdeck", FALSE, TRUE))


  ######################################################################################
  ##                SERVER ADMIN SETTINGS
  ######################################################################################
  ADMIN<-reactiveValues()
  # filepath for admin
  fpadm<-reactiveVal(NULL)
  # filepath creation
  observe({
    appdir<-file.path(tools::R_user_dir("susoquestionnairemanual", which = "data"), "admin")
    if(!dir.exists(appdir)){
      dir.create(appdir, recursive = TRUE, showWarnings = FALSE)
    }
    admfile<-file.path(appdir, "admin_settings.rds")
    fpadm(admfile)
  })
  # Check if server settings are stored & work
  fields<-reactiveVal(c("suso.server", "suso.user", "suso.pass", "suso.workspace"))
  observe({
    admfile<-req(fpadm())
    if(file.exists(admfile)){
      tmp.admin<-readRDS(paste0(admfile))
    }  else {
      tmp.admin<-character(0)
    }

    if (length(tmp.admin)==length(fields())){
      admin.vars<-tmp.admin
      suso_set_key(admin.vars[["suso.server"]], admin.vars[["suso.user"]], admin.vars[["suso.pass"]])
      # check credentials and set to TBD if wrong
      waiter::waiter_show(
        color = "rgba(13, 71, 161, 0.7)",
        html = tagList(
          spin_fading_circles(),
          "Checking Credentials ..."
        )
      )
      credcheck<-suso_PwCheck(workspace = admin.vars[["suso.workspace"]])$status_code[1]
      waiter::waiter_hide()
      if (credcheck==200) {
        ADMIN$settings<-admin.vars
        shinyjs::disable("serversettingsdiv")
        shinyjs::enable("serversettingserasediv")
      } else {
        admin.vars<-rep("TBD", length(admin.vars))
        names(admin.vars)<-fields()
        shiny::showNotification("Wrong Credentials")
        ADMIN$settings<-NULL

        req(FALSE)
      }

    } else {
      admin.vars<-c(rep("TBD", length(fields())))
      names(admin.vars)<-fields()
      shiny::showNotification("Some credentials are missing. Please reset!")
      ADMIN$settings<-NULL
      req(FALSE)
    }


  }, priority = 0)

  ##  ii) write to file in data/admin/admin_settings.rds

  observeEvent(input$suso.save, {
    #admin.vars.new<-ADMIN$settings
    admfile<-req(fpadm())
    admin.vars <- sapply(fields(), function(x) input[[x]])
    names(admin.vars)<-fields()

    if(file.exists(admfile)){
      try(
        {file.remove(admfile)},
        silent = T
      )
    }
    suso_clear_keys()
    suso_set_key(admin.vars[["suso.server"]], admin.vars[["suso.user"]], admin.vars[["suso.pass"]])
    ##  2. Check settings again
    waiter::waiter_show(
      color = "rgba(13, 71, 161, 0.7)",
      html = tagList(
        spin_fading_circles(),
        "Checking Credentials ..."
      )
    )
    credcheck<-suso_PwCheck(workspace = admin.vars[["suso.workspace"]])$status_code[1]
    waiter::waiter_hide()

    if (credcheck==200) {
      saveRDS(admin.vars, admfile)
      shinyjs::disable("serversettingsdiv")
      shinyjs::enable("serversettingserasediv")
      ADMIN$settings<-admin.vars

    } else {
      admin.vars<-rep("TBD", length(admin.vars))
      names(admin.vars)<-fields()
      shiny::showNotification("Wrong Credentials")
      ADMIN$settings<-NULL
      req(FALSE)
    }
    ##  2. Hand Over Settings

  }, priority = -2, ignoreInit = T)

  ##  iv) erase settings
  observeEvent(input$suso.erase, {
    admfile<-req(fpadm())
    try(
      {file.remove(admfile)},
      silent = T
    )
    shinyjs::enable("serversettingsdiv")
    shinyjs::disable("serversettingserasediv")
    ADMIN$settings<-NULL

    # update selection
    update_material_dropdown(session = session,
                             input_id = "susoQuestionnaire",
                             choices = c("NONE LOADED"),
                             value = "NONE LOADED")
  }, ignoreInit = T)

  ##  3. Load the questionnaires for selection
  questionnaires <- reactiveValues()
  observe(
    {
      shiny::validate(need(input$dataLoad == "Server", message = F))
      settings<-req(ADMIN$settings)
      if (SurveySolutionsAPI::suso_PwCheck(
        settings[["suso.server"]], settings[["suso.user"]], settings[["suso.pass"]], settings[["suso.workspace"]]
      )$status_code[1]!=200) {

        settings<-rep("TBD", length(fields()))
        names(settings)<-fields()
        req(FALSE)
      }

      ##  if settings check is ok, questionnaires are loaded.
      if (sum(grepl(x=settings,pattern = "TBD"))==0){
        apicall<-SurveySolutionsAPI::suso_getQuestDetails(workspace = settings[["suso.workspace"]])
        # if no data, stop and show message
        if(length(apicall)==1 | is.null(apicall)) {
          showNotification("No questionnaire loaded on the provided server/workspace. Please import questionnaires first!", type = "error")
          req(FALSE)
        } else {
          tab<-data.table(apicall,
                          key = c("Title", "Version"))
        }



        tab[,c("date", "time"):=tstrsplit(LastEntryDate, "T", fixed=TRUE)][]
        tab[,time:=as.ITime(time)]
        tab[,date:=as.IDate(date)]
        tab[,QuestionnaireIdVersion:=sprintf("%s_%d", QuestionnaireId, Version)]
        setorderv(tab, c("date", "time"), -1)
        dropDown<-sprintf("(ver. %d) %s", tab$Version, tab$Title)

        dropDown<-setNames(object = c("",tab$QuestionnaireIdVersion), c("",dropDown))

        update_material_dropdown(session = session,
                                 input_id = "susoQuestionnaire",
                                 choices = dropDown,
                                 value = dropDown[1])
        questionnaires$tab<-tab
      }
    })


  ##  6. Refresh now
  observeEvent(input$suso.refresh.now,
               {
                 ## add refresh button action here
               },
               ignoreInit = F
  )
  ######################################################################################
  ##                OWNTRACKS ADMIN SETTINGS
  ######################################################################################
  userListOwnTracks <- reactiveValues()
  ##  4. Load the TEAMS for selection
  questionnaires <- reactiveValues()
  # observeEvent(input$pass, {
  #   settings <- ADMIN$settings
  #   shiny::validate(
  #     need(settings, message = "No Server Settings Provided"),
  #     need(input$pass == "Tracker1234" | input$pass == "RomaniaLFS2018", message = F)
  #   )
  #   shiny::validate(need(sum(grepl(x = settings, pattern = "TBD")) == 0, message = "No Server Settings Provided"))
  #   tab <- data.table(getSV(
  #     url = settings[["suso.server"]],
  #     usr = settings[["suso.user"]],
  #     pass = settings[["suso.pass"]]
  #   )$Users, key = c("UserName", "UserId"))
  #
  #   dropDown <- setNames(object = tab$UserId, tab$UserName)
  #   update_material_dropdown(
  #     session = session,
  #     input_id = "teamTrack",
  #     choices = dropDown,
  #     value = dropDown[1]
  #   )
  # })

  # observeEvent(input$genTeamTrack, {
  #   team <- input$teamTrack
  #   owConfig <- owntrConfigSample
  #   shiny::validate(need(team != "No Data Loaded!", message = F))
  #   settings <- ADMIN$settings
  #   shiny::validate(need(settings, message = "No Server Settings Provided"))
  #   tab <- data.table(getINT(
  #     url = settings[["suso.server"]], sv = team,
  #     usr = settings[["suso.user"]],
  #     pass = settings[["suso.pass"]]
  #   )$Users, key = c("UserName", "UserId"))
  #   ## make sure that interviewer exist at all
  #   shiny::validate(need(nrow(tab) > 0, message = "No Interviewer in the team"))
  #   ##########################################
  #   ##  Generat individual owntracks fies
  #   # print(tab)
  #   fullOwnConfig <- list()
  #   owntrUsers <- list()
  #   for (i in 1:length(tab$UserName)) {
  #     owntrUsers[[i]] <- data.table(team, UserName = tab$UserName[i], UserId = tab$UserId[i], tid = paste0("i", i))
  #     owConfig$deviceId <- team
  #     owConfig$tid <- paste0("i", i)
  #     fullOwnConfig[[paste0("i", i)]] <- toJSON(owConfig)
  #   }
  #   owntrUsers <- rbindlist(owntrUsers)
  #   userListOwnTracks$config <- fullOwnConfig
  #   userListOwnTracks$users <- owntrUsers
  #   ##  Write files and team names to disk
  # })
  #
  # #######################
  # ##  5. Download Owntracks
  # output$downloadOwnTrConfig <- downloadHandler(
  #   filename = function() {
  #     paste("OwnTracksConfig-", input$teamTrack, ".zip", sep = "")
  #   },
  #   content = function(file) {
  #     ##  i. create temp dir, and set as wd, create folder inside (DSN)
  #     temp.dir <- tempdir()
  #     wdOld <- getwd()
  #     setwd(temp.dir)
  #     DSN <- paste0("ConfigFiles")
  #     if (!dir.exists(DSN)) dir.create(DSN, recursive = T)
  #     allFiles <- userListOwnTracks$config
  #     shiny::validate(need(allFiles, message = F))
  #     all_names <- names(allFiles)
  #     ##  ii. write all file to DSN folder
  #     for (i in all_names) {
  #       path <- file.path(DSN, paste0(i, ".otrc"))
  #       write_file(x = allFiles[[i]], path = path)
  #     }
  #     ##  iii. list the files in DSN folder
  #     fs <- list.files(DSN, full.names = T)
  #     zip(zipfile = file, files = fs)
  #     setwd(wdOld)
  #   }, contentType = "application/zip"
  # )

  #######################
  ##  6. GPS logger files
  ##    - does not require a username (except for ssl and with pw.)
  ##    - only active when there is gpslogger active

  ######################################################################################
  ##                PARADATA
  ######################################################################################
  ############ 1. Load Paradata ##########################
  # zip file module
  dataFileZip<-zipFileInput_server(
    id = "file1",
    sep = "\t",
    zipInput = T,
    colClasses = list(
      character=c(1,3,4,7,8),
      integer=c(2,5),
      POSIXct = c(6)
    )
  )

  paraDataFile <- reactive({
    if (input$dataLoad == "File") {
      ############ A. From File
      withProgress(
        message = "Data Upload in Progress",
        detail = "This may take a while...",
        value = 0,
        {
          dataFile<-req(dataFileZip())
          CHECKdataFile<<-dataFile
          unpack <- function(parafile = NULL) {
            if (length(parafile) == 9) {
              ## OLD format
              names(parafile) <- c("interview__id", "counter", "action", "responsible", "role", "time", "var", "var_resp", "rid")
              resps <- parafile[, tstrsplit(var_resp, "(\\|)|(,)", fixed = F, names = T, fill = "<NA>")][]
              splits <- (length(resps))
              # parafile[,response1:=var_resp]
              parafile[, c(paste0("response", 1:(length(resps)))) := resps]
              rm(resps)
            } else if (length(parafile) == 8) {
              names(parafile) <- c("interview__id", "counter", "action", "responsible", "role", "time", "tz", "var_resp")
              ## get responses only for AnswerSet events
              #parafile_as<-parafile[action=="AnswerSet"]
              # parafile_nas<-parafile[action!="AnswerSet"]
              resps <- parafile[, tstrsplit(var_resp, "||", fixed = T, names = T, fill = "<NA>")][]
              if(length(resps)==2) {
                # No roster
                resps1 <- resps[, tstrsplit(V2, "(\\|)|(,)", fixed = F, names = T, fill = "<NA>")][]
                splits <- (length(resps1))
                parafile[, c("var") := resps[, .(V1)]]
                parafile[, c(paste0("response", 1:(length(resps1)))) := resps1]
                # set roster id to 0
                parafile[, c(paste0("rid", 1:(length(resps1)))) := 0]

              } else if(length(resps)>=3) {
                # with roster
                resps1 <- resps[, tstrsplit(V2, "(\\|)|(,)", fixed = F, names = T, fill = "<NA>")][]
                resps2 <- resps[, tstrsplit(V3, "(,)", fixed = F, names = T, fill = "<NA>")][]
                splits <- (length(resps1))
                parafile[, c("var") := resps[, .(V1)]]
                parafile[, c(paste0("response", 1:(length(resps1)))) := resps1]
                parafile[, c(paste0("rid", 1:(length(resps2)))) := resps2]
                # clean up
                rm(resps); rm(resps1); rm(resps2); gc(verbose = F)
              }

            }
            for (i in 1:splits) {
              i <- paste0("response", i)
              parafile[, c(i) := as.factor(get(i))]
            }
            # suppressWarnings(
            #   parafile[,time:=as.ITime(time)]
            # )
            #parafile[, c("date", "time") := tstrsplit(time, "T", fixed = TRUE)][]
            #parafile[,]
            # replace missing tz
            stat_mode<-function(x) {
              # mode solution from: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
              ux<-unique(x)
              ux[which.max(tabulate(match(x, ux)))]
            }
            parafile[,tz:=dplyr::if_else(is.na(tz), stat_mode(tz), tz)]
            # convert
            suppressWarnings(
              suppressMessages(
                parafile[,dateTime:=lubridate::as_datetime(time)-hms(tz)][,c("tz"):=NULL]
              )
            )
            parafile[,time:=as.ITime(dateTime, tz = "UTC")]
            parafile[,date:=as.IDate(dateTime, tz = "UTC")]
            parafile[,wDAY:=wday(date)]
            parafile[,mDAY:=mday(date)]
            parafile[,MONTH:=month(date)]
            parafile[,WEEK:=isoweek(date)]

            parafile[, role:=as.factor(role)]
            parafile[, action:=as.factor(action)]
            parafile[, responsible:=as.factor(responsible)][]

            setkeyv(parafile, c("interview__id", "responsible"))
            return(parafile)
          }


          dataFile <- tryCatch(
            {
              unpack(parafile = dataFile)
            },
            error = function(e) {
              shinyalert("Data Load Error!",
                         paste("File must be exactly the same as downloaded from your Survey Solutions Server", e),
                         type = "error")
              req(F)
            }
          )
          incProgress(amount = 0.25, message = "Transformation completed")
        }
      )


      return(dataFile)
    } else if (input$dataLoad == "Server") {
      withProgress(
        message = "Data import from Server",
        detail = "This may take a while...",
        value = 0,
        {
          ############ B. From Server
          settings <- req(ADMIN$settings)
          if (SurveySolutionsAPI::suso_PwCheck(
            settings[["suso.server"]], settings[["suso.user"]], settings[["suso.pass"]], settings[["suso.workspace"]]
          )$status_code[1]!=200) {

            settings<-rep("TBD", length(fields()))
            names(settings)<-fields()
            req(FALSE)
          }

          # shiny::validate(
          #   need(settings, message = "No Server Settings Provided"),
          #   need(sum(grepl(x = admin.vars, pattern = "TBD")) == 0, message = "No Server Settings Provided")
          # )
          tab <- questionnaires$tab
          shiny::validate(need(tab, message = F))
          # get quid, pattern of string is 'qid_version'
          qid <- stringr::str_split(input$susoQuestionnaire, pattern = stringr::fixed("_"), simplify = T)[1]
          # get version, pattern of string is 'qid_version'
          v <- as.numeric(stringr::str_split(input$susoQuestionnaire, pattern = stringr::fixed("_"), simplify = T)[2])
          shiny::validate(
            need(input$susoQuestionnaire != "Specify Server Settings First", message = F),
            need(input$susoQuestionnaire != "", message = F)
          )

          incProgress(amount = 0.25, message = "Connection Established")
          dataFile<-SurveySolutionsAPI::suso_export_paradata(
            workspace = settings[["suso.workspace"]],
            questID = qid,
            version = v,
            workStatus = "Completed",
            reloadTimeDiff = 1,
            inShinyApp = T,
            multiCore = NULL,
            onlyActiveEvents = FALSE,
            allResponses = T,
            gpsVarName = NA,
            verbose = T,
            showProgress = F
          )
          incProgress(amount = 0.25)
        }
      )
      return(dataFile)
    } else if (input$dataLoad == "LocalFile") {
      ############ A. From File
      withProgress(
        message = "Data Upload in Progress",
        detail = "This may take a while...",
        value = 0,
        {
          #######################################
          ## ONLY TEMPORARY SOLUTION
          ## --> DATABASE CONNECTION REQUIRED
          incProgress(amount = 0.1)
          dataFile <- fst::read.fst("data/pilot/paradata_d3.fst", as.data.table = T)
          ## Modifications
          dataFile <- dataFile[action != "", ][, action := droplevels(action)]
          dataFile[, rid := counter]

          incProgress(amount = 0.25, message = "Transformation completed")
        }
      )

      # CHECKfile<-dataFile
      return(dataFile)

    }
  })

  ########### 2. Transform Paradata  ##########################
  observe({
    dataFile <- req(paraDataFile())
    if(input$dataLoad %in% c("LocalFile", "File")){
      #paradata_files<-paradata_files$AnswerSet
      ## A add rid if it doesnt exist
      #if (length(grep("rid", names(paradata_files))) == 0) paradata_files[, rid := 0]

      ##  List for subsets
      paraSubsets <- list()

      ##################################################################
      ##  2.1. Get Start/End date & DATE seletion (causes re-calculation)
      fromDate <- as.character(min(dataFile$date, na.rm = T), "%d %B, %Y")
      toDate <- as.character(max(dataFile$date, na.rm = T), "%d %B, %Y")
      update_material_date_picker(
        session,
        input_id = "dateFrom",
        value = fromDate
      )
      update_material_date_picker(
        session,
        input_id = "dateTo",
        value = toDate
      )
      setnames(dataFile, "rid1", "rid")
      system.time(para_data<-paraTransformation(paradata_files = dataFile, multiCore = NULL))
      # ##################################################################
      # ##  2.2. GET ALL ACTION COUNTS
      # actionDistr <- as.data.table(table(paradata_files$action))
      # setorderv(actionDistr, "N", order = -1)
      # names(actionDistr) <- c("Action", "Count")
      # paraSubsets$action <- actionDistr
      # ##################################################################
      # ##  2.3. GET ALL RESPONSIBLE COUNTS
      # userDistr <- as.data.table(table(paradata_files$responsible))
      # setorderv(userDistr, "N", order = -1)
      # names(userDistr) <- c("Responsible", "Count")
      # userDistr <- userDistr[(Responsible != ""), ]
      # paraSubsets$user <- userDistr
      #
      # ##################################################################
      # ##  2.4. GET ALL ROLE COUNTS (0= system, 1 interviewer, 2 sv, 3 hq)
      # roleDistr <- as.data.table(table(droplevels(paradata_files$role)))
      # setorderv(roleDistr, "N", order = -1)
      # names(roleDistr) <- c("Role", "Count")
      # paraSubsets$role <- roleDistr
      # para_data <- list()
      #
      # ##  2.5. Extract questionnaire ID and Key
      # KeyAssigned <- paradata_files[action == "KeyAssigned"][, c("responsible", "role", "var_resp", "rid") := NULL]
      # setnames(KeyAssigned, "var", "key")
      # KeyAssigned <- droplevels(KeyAssigned)
      # paradata_files <- paradata_files[action != "KeyAssigned"]
      # paradata_files <- droplevels(paradata_files)
      # KeyAssigned <- KeyAssigned[, .SD[1], by = .(interview__id)]
      # KeyAssigned_merge <- KeyAssigned[, .(interview__id, key)]
      # setkeyv(KeyAssigned, "interview__id")
      # para_data$KeyAssigned <- KeyAssigned
      # ##  2.6. Comments
      # CommentSet <- paradata_files[action == "CommentSet"]
      # if (nrow(CommentSet) > 0) {
      #   setnames(CommentSet, "var_resp", "comment")
      #   CommentSet <- droplevels(CommentSet)
      #   paradata_files <- paradata_files[action != "CommentSet"]
      #   paradata_files <- droplevels(paradata_files)
      #   para_data$CommentSet <- CommentSet
      # }
      # ## 2.7 Completed
      # Completed <- paradata_files[action == "Completed"][, c("responsible", "role", "var_resp", "rid") := NULL]
      # if (nrow(Completed) > 0) {
      #   setnames(Completed, "var", "comment")
      #   Completed <- droplevels(Completed)
      #   paradata_files <- paradata_files[action != "Completed"]
      #   paradata_files <- droplevels(paradata_files)
      #   para_data$Completed <- Completed
      # }
      # ##  2.8. AnswerSet
      # para1_answer <- paradata_files[action == "AnswerSet" | action == "Paused"]
      # para1_answer <- droplevels(para1_answer)
      #
      # ##  3. Time Difference (SORT by counter)
      # ##  3.1. Function (use shift/lead, and check lead date is the same)
      # calcTimeDiff <- function(DTfile, by = c("interview__id", "DAY", "MONTH")) {
      #   DTfile <- copy(DTfile)
      #   setorderv(DTfile, c("interview__id", "date", "time", "counter"))
      #   # DTfile<-DTfile[,.SD[.N>20], by=.(interview__id)]
      #   DTfile[, resp_time := as.integer(0)][, resp_time := ifelse((data.table::shift(date, type = "lag") == date & action != "Paused"),
      #     time - data.table::shift(time, type = "lag"), NA
      #   ), by = .(interview__id)][]
      #   DTfile[, breaks := ifelse(resp_time > 120 & !is.na(resp_time), 1, 0), by = .(interview__id)]
      #   return(DTfile)
      # }
      # para1_answer <- calcTimeDiff(para1_answer)
      # ##
      # para1_answer <- para1_answer[!is.na(breaks)]
      # para1_answer[, duration := round((sum(resp_time, na.rm = T)) / 60, 2), by = .(interview__id)]
      # para1_answer[breaks == 0, durationNOBREAK := round((sum(resp_time, na.rm = T)) / 60, 2), by = .(interview__id)]
      # para1_answer[, m_resp_time_varTRIM := (mean(resp_time, na.rm = T, trim = 0.05)), by = .(var)]
      # para1_answer[, m_resp_time_var := (mean(resp_time, na.rm = T)), by = .(var)]
      # para1_answer[breaks == 0, m_diff_dev := resp_time - m_resp_time_varTRIM]
      # para1_answer[, start := min(time, na.rm = T), by = .(interview__id)]
      # para1_answer[, startHour := min(hour(time), na.rm = T), by = .(interview__id)]
      # para1_answer <- droplevels(para1_answer)
      # para1_answer[, var := factor(var)]
      #
      # para1_answer_merge <- para1_answer[, .SD[1], by = .(interview__id, role)]
      # para1_answer_merge <- para1_answer_merge[, .(interview__id, responsible, role)]
      # ##########################################################################
      # ##  2. GPS extract -->if no name, try identification through grepl
      # ##      SELECTION SELECTION
      # gpsVarName <- ifelse(input$dataLoad == "LocalFile", "g_geoLocation", NA)
      # allResponses <- TRUE
      #
      # varNames <- levels(para1_answer$var)
      # if (is.na(gpsVarName)) {
      #   gpsVarMain <- varNames[grepl("gps", varNames)]
      # } else {
      #   stopifnot(is.character(gpsVarName), gpsVarName %in% varNames)
      #   gpsVarMain <- gpsVarName
      # }
      #
      # ## create gps file when exists
      # if (length(gpsVarMain) > 0) {
      #   ## Select first gps variable
      #   cat("\nExtracting GPS variable.\n")
      #   gpsVar <- gpsVarMain[1]
      #   gps_file <- para1_answer[var == gpsVar]
      #   if (nrow(gps_file) == 0) stop(cat("No GPS values found with: ", gpsVarName))
      #   if (!allResponses) {
      #     gp <- gps_file[, tstrsplit(response, ",", fixed = T, fill = "<NA>", names = TRUE)][]
      #     gps_file <- cbind(gps_file, gp)
      #     setnames(gps_file, c("V1", "V2"), c("response1", "response2"))
      #   }
      #
      #   gps_file <- gps_file[, .(
      #     interview__id, responsible, time, var_resp, var,
      #     date, durationNOBREAK, response1, response2
      #   )]
      #   gps_file <- gps_file[, c("long") := tstrsplit(response2, "[", fixed = T, keep = c(1))][]
      #   gps_file[, lat := as.numeric(as.character(response1))]
      #   gps_file[, long := as.numeric(as.character(long))]
      #   gpsSelect <- sum(!is.na(gps_file$lat))
      #   ## If empty iterate over next/only if length>1/until length==k
      #   k <- 2
      #   while (gpsSelect >= 0 & gpsSelect <= nrow(gps_file) & length(gpsVarMain) > 1 & length(gpsVarMain) != k) {
      #     gpsVar <- gpsVarMain[k]
      #     gps_file <- para1_answer[var == gpsVar]
      #     if (!allResponses) {
      #       gp <- gps_file[, tstrsplit(response, ",", fixed = T, fill = "<NA>", names = TRUE)][]
      #       gps_file <- cbind(gps_file, gp)
      #       setnames(gps_file, c("V1", "V2"), c("response1", "response2"))
      #     }
      #     gps_file <- gps_file[, .(
      #       interview__id, responsible, time, var_resp,
      #       date, durationNOBREAK, response1, response2
      #     )]
      #     gps_file <- gps_file[, c("long") := tstrsplit(response2, "[", fixed = T, keep = c(1))][]
      #     gps_file[, lat := as.numeric(as.character(response1))]
      #     gps_file[, long := as.numeric(as.character(long))]
      #     k <- k + 1
      #     gpsSelect <- sum(!is.na(gps_file$lat))
      #   }
      #   ##  For merge with EVENT data
      #   gps_file_merge <- gps_file[, .(interview__id, lat, long)]
      #   gps_file_merge <- gps_file_merge[, .SD[1], by = .(interview__id)]
      #   setkeyv(gps_file_merge, "interview__id")
      # }
      # ##  Subset with function, key and lapply
      # ## loop over levels of action with LAPPLY
      # ## a<-lapply(levels(CHECK$action), FUN = subsetDataTableAction, CHECK)
      # ##  not used for now
      # subsetDataTableAction <- function(dt, x) {
      #   setkeyv(x, "action")
      #   ## print(dt)
      #   file <- x[dt]
      #   ## print(file)
      #   return(file)
      # }
      # AnswerSet <- para1_answer
      # AnswerSet <- AnswerSet[!is.na(interview__id)]
      # setkeyv(AnswerSet, "interview__id")
      # if (exists("gps_file_merge")) AnswerSet <- gps_file_merge[AnswerSet, on = "interview__id"]
      # AnswerSet <- KeyAssigned_merge[AnswerSet, on = "interview__id"]
      # para_data$AnswerSet <- AnswerSet
      #
      # ##  2.9. Answer Removed (COUNT the number of Removed answer by questionnaire)
      # AnswerRemoved <- paradata_files[action == "AnswerRemoved"]
      # AnswerRemoved <- AnswerRemoved[!is.na(interview__id)]
      # AnswerRemoved[, count := length(counter), by = interview__id]
      # AnswerRemoved[, c("responsible", "role") := NULL]
      # AnswerRemoved <- droplevels(AnswerRemoved)
      # AnswerRemoved <- merge(AnswerRemoved, para1_answer_merge, by = "interview__id", allow.cartesian = T)
      # setkeyv(AnswerRemoved, "interview__id")
      # if (exists("gps_file_merge")) AnswerRemoved <- gps_file_merge[AnswerRemoved, on = "interview__id"]
      # AnswerRemoved <- KeyAssigned_merge[AnswerRemoved, on = "interview__id"]
      # para_data$AnswerRemoved <- AnswerRemoved
      # ##  2.10. Approved
      # ApproveByHeadquarter <- paradata_files[action == "ApproveByHeadquarter"]
      # ApproveByHeadquarter <- droplevels(ApproveByHeadquarter)
      # ApproveBySupervisor <- paradata_files[action == "ApproveBySupervisor"]
      # ApproveBySupervisor <- droplevels(ApproveBySupervisor)
      # ##  2.11. Invalid
      # QuestionDeclaredInvalid <- paradata_files[action == "QuestionDeclaredInvalid"]
      # QuestionDeclaredInvalid <- QuestionDeclaredInvalid[!is.na(interview__id)]
      # QuestionDeclaredInvalid[, count := length(counter), by = interview__id]
      # setkeyv(QuestionDeclaredInvalid, "interview__id")
      # if (exists("gps_file_merge")) QuestionDeclaredInvalid <- gps_file_merge[QuestionDeclaredInvalid, on = "interview__id"]
      # QuestionDeclaredInvalid <- KeyAssigned_merge[QuestionDeclaredInvalid, on = "interview__id"]
      # para_data$QuestionDeclaredInvalid <- QuestionDeclaredInvalid
      # ##  2.12. Valid
      # QuestionDeclaredValid <- paradata_files[action == "QuestionDeclaredValid"]
      # QuestionDeclaredValid <- QuestionDeclaredValid[!is.na(interview__id), ]
      # QuestionDeclaredValid[, count := length(counter), by = interview__id]
      # para_data$QuestionDeclaredValid <- QuestionDeclaredValid
      # ##  2.13 Restarted
      # Restarted <- paradata_files[action == "Restarted"]
      # Restarted <- Restarted[!is.na(interview__id), ]
      # Restarted[, count := length(counter), by = interview__id]
      # setkeyv(Restarted, "interview__id")
      # if (exists("gps_file_merge")) Restarted <- gps_file_merge[Restarted, on = "interview__id"]
      # Restarted <- KeyAssigned_merge[Restarted, on = "interview__id"]
      # para_data$Restarted <- Restarted
      #
      # ##  2.14. Rejected
      # Reject <- paradata_files[action == "RejectedBySupervisor" | action == "RejectedByHeadquarter"][, c("var_resp", "rid") := NULL]
      # setnames(Reject, "var", "comment")
      # Reject <- droplevels(Reject)
      # # paradata_files<-paradata_files[action!="RejectedBySupervisor"&action!="RejectedByHeadquarter"]
      # # paradata_files<-droplevels(paradata_files)
      # setkeyv(Reject, "interview__id")
      # if (exists("gps_file_merge")) Reject <- gps_file_merge[Reject, on = "interview__id"]
      # Reject <- KeyAssigned_merge[Reject, on = "interview__id"]
      # para_data$Reject <- Reject

      ##  Export DATA
      ch <- names(para_data)
      ch <- setNames(object = ch, ch)
      update_material_dropdown(
        session,
        input_id = "file_select_view",
        choices = ch,
        value = ch["AnswerSet"]
      )
      material_spinner_hide(session, "viewData")

      para_data_coll$para_data <- para_data
    } else {
      # From SERVER-->no manipulation
      ##  Export DATA
      ch <- names(dataFile)
      ch <- setNames(object = ch, ch)
      update_material_dropdown(
        session,
        input_id = "file_select_view",
        choices = ch,
        value = ch["AnswerSet"]
      )

      ##################################################################
      ##  2.1. Get Start/End date & DATE seletion (causes re-calculation)
      fromDate <- as.character(min(dataFile$AnswerSet$date, na.rm = T), "%d %B, %Y")
      toDate <- as.character(max(dataFile$AnswerSet$date, na.rm = T), "%d %B, %Y")
      update_material_date_picker(
        session,
        input_id = "dateFrom",
        value = fromDate
      )
      update_material_date_picker(
        session,
        input_id = "dateTo",
        value = toDate
      )
      para_data_coll$para_data<-dataFile
    }
  })

  ######################################################
  ## PLOTS
  ##    3 Sections:
  ##        1. Questionnaire
  ##        2. Interviewer
  ##        3. MAP
  ######################################################
  ##  1. Average response time in Questionnaire Proces
  output$timePlot <- renderPlotly({
    parafile <- para_data_coll$para_data$AnswerSet
    shiny::validate(need(parafile, message = F))
    material_spinner_show(session, "timePlot")
    #############################
    ##  1. Exclude Outliers

    parafile <- parafile[breaks == 0]
    parafile <- parafile[!is.na(resp_time)]
    parafile <- parafile[resp_time <= 120]
    ##  2. Transform Data
    ##  2.1. Get Question order based on median
    parafile <- parafile[!is.na(counter)]
    CHECKparafile<<-parafile
    qPos <- parafile[, .(counterMedian = median(counter, na.rm = T)), by = .(var)]
    setkeyv(qPos, "var")
    setkeyv(parafile, "var")
    parafile <- qPos[parafile, on = "var"]
    ## 2.2. Get slider inputs
    #TL <- ifelse(length(unique(CHECKparafile$WEEK)) > 1, "WEEK", NULL)

    #shiny::validate(need(!is.null(TL), message = "Number of provided observerations not sufficient!"))
    if (length(unique(parafile$WEEK)) > 1) {
      TL<-"WEEK"
      aval1 <- list()
      k <- 1
      for (step in unique((parafile$WEEK))) {
        ## i. subset data and aggregate
        # print(step)
        parafile_tmp <- subset(parafile, WEEK == step)

        parafile_tmp <- parafile_tmp[, .(
          Av_ResponseTime = round(mean(resp_time, na.rm = T), 2),
          Av_Duration = round(mean(m_resp_time_varTRIM, na.rm = T), 2),
          N_obs = n_distinct(interview__id)
        ), by = .(WEEK, counterMedian, var)]

        setorderv(parafile_tmp, c("counterMedian"))
        parafile_tmp <- parafile_tmp[!is.na(counterMedian) & !is.nan(Av_ResponseTime)]
        parafile_tmp[, counter := 1:.N]
        setnames(parafile_tmp, "counter", "QuestionnaireProgression")
        # if (length(parafile_tmp[,is.na(QuestionnaireProgression)])==nrow(parafile_tmp) |
        #     length(parafile_tmp[,is.na(Av_ResponseTime)])==nrow(parafile_tmp)) next()
        ## ii. creat the input data list
        # if(length(parafile_tmp$QuestionnaireProgression)==0) k=k+1; next()

        aval1[[k]] <- list(
          visible = FALSE,
          name = paste0(step),
          x = parafile_tmp$QuestionnaireProgression,
          y = parafile_tmp$Av_ResponseTime,
          var = as.character(parafile_tmp$var)
        )
        aval1[k][[1]]$visible <- TRUE

        k <- k + 1
      }
    } else {
      TL <- NULL
      parafile <- parafile[, .(
        Av_ResponseTime = round(mean(resp_time, na.rm = T), 2),
        Av_Duration = round(mean(m_resp_time_varTRIM, na.rm = T), 2),
        N_obs = n_distinct(interview__id)
      ), by = .(counterMedian, var)]

      setorderv(parafile, c("counterMedian"))
      parafile <- parafile[!is.na(counterMedian) & !is.nan(Av_ResponseTime)]
      parafile[, counter := 1:.N]
      setnames(parafile, "counter", "QuestionnaireProgression")
      aval1 <- copy(parafile)
    }
    # ## 2.3. Get all inputs
    material_spinner_hide(session, "timePlot")

    ## With slider
    CHECK <- aval1
    p <- plotlyLine(
      data_in = aval1, x = "QuestionnaireProgression", y = "Av_ResponseTime",
      title = "Time over Questionnaire Progress",
      x_title = "Questionnair Progress",
      y_title = "Average Response Time (in Sec.)",
      x_lab_axis_txt = "var",
      x_lab_axis_val = "x",
      timeLine = TL
    )
    return(p)
  })

  ######################################################
  ##  2. Average Questionnaire Duration by Interviewer (30 fastest)
  output$respPlot <- renderPlotly({
    parafile <- para_data_coll$para_data$AnswerSet
    shiny::validate(
      need(parafile, message = F),
      need(nrow(parafile) > 0, message = F)
    )
    material_spinner_show(session, "respPlot")
    parafile <- parafile[, .(Av_Duration = mean(durationNOBREAK, na.rm = T)), by = .(responsible)]

    ##  Overall mean
    durMean <- mean(parafile$Av_Duration, na.rm = T)
    ##  fastest 30
    setorderv(parafile, "Av_Duration",
              order = ifelse(input$respfastSlow == "Fast1", 1, -1)
    )
    fast30 <- parafile[1:30]
    ##  slowest 30
    setorderv(parafile, "Av_Duration", -1)
    slow30 <- parafile[1:30]
    material_spinner_hide(session, "respPlot")
    p <- plotlyBarH(
      data_in = fast30, x = "Av_Duration", y = "responsible",
      title = "Average Questionnaire Completion Time",
      ordered_y = TRUE,
      x_title = "Average Completion Time (in min.)"
    )
    return(p)
  })

  ######################################################
  ##  3. Total Interview Competion time (fastest 30)
  output$qTotPlot <- renderPlotly({
    parafile <- para_data_coll$para_data$AnswerSet
    shiny::validate(
      need(parafile, message = F),
      need(nrow(parafile) > 0, message = F)
    )
    material_spinner_show(session, "qTotPlot")
    parafile <- parafile[resp_time <= 120]
    parafile <- parafile[, .(Total_Responsetime = sum(resp_time, na.rm = T) / 60), by = .(key)]
    parafile <- parafile[Total_Responsetime > 0]
    ##  User can change order
    setorderv(parafile, "Total_Responsetime",
              order = ifelse(input$shortLong == "Shortest", 1, -1)
    )
    fast30 <- parafile[1:30]
    material_spinner_hide(session, "qTotPlot")
    p <- plotlyBarH(
      data_in = fast30, x = "Total_Responsetime", y = "key",
      title = "Time per Interview (shortest/longest)",
      ordered_y = TRUE,
      x_title = "Total Response Time (in min.)"
    )
    return(p)
  })
  ######################################################
  ##  4. Fastest Actor
  output$DevPlot <- renderPlotly({
    parafile <- para_data_coll$para_data$AnswerSet
    shiny::validate(
      need(parafile, message = F),
      need(nrow(parafile) > 0, message = F)
    )
    material_spinner_show(session, "DevPlot")
    parafile <- parafile[responsible != ""]
    parafile <- parafile[, .(Mean_Deviation = mean(m_diff_dev, na.rm = T)), by = .(responsible)]
    parafile <- parafile[!is.nan(Mean_Deviation)]
    setorderv(parafile, "Mean_Deviation",
              order = ifelse(input$devfastSlow == "Fast2", 1, -1)
    )
    fast30 <- parafile[1:30]
    material_spinner_hide(session, "DevPlot")
    p <- plotlyBarH(
      data_in = fast30, x = "Mean_Deviation", y = "responsible",
      title = "Average Pace",
      ordered_y = TRUE,
      x_title = "Deviation from Question Mean (in Sec.)"
    )
    return(p)
  })
  ######################################################
  ##  5. Answer Removed Int
  output$answRemInt <- renderPlotly({
    parafile <- para_data_coll$para_data$AnswerRemoved
    shiny::validate(
      need(parafile, message = F),
      need(nrow(parafile) > 0, message = F)
    )
    material_spinner_show(session, "answRemInt")
    parafile <- parafile[responsible != ""]
    parafile <- parafile[, .(Removals = length(key)), by = .(responsible)]
    setorderv(parafile, "Removals",
              order = ifelse(input$answRemHighLow == "Most3", -1, 1)
    )
    fast30 <- parafile[1:30]
    material_spinner_hide(session, "answRemInt")
    p <- plotlyBarH(
      data_in = fast30, x = "Removals", y = "responsible",
      title = "Number of Response Removals",
      ordered_y = TRUE,
      x_title = "Number of Removals"
    )
    return(p)
  })
  ######################################################
  ##  6. Questionnaire Invalid
  output$invalidQuest <- renderPlotly({
    parafile <- para_data_coll$para_data$QuestionDeclaredInvalid
    shiny::validate(
      need(parafile, message = F),
      need(nrow(parafile) > 0, message = F)
    )
    material_spinner_show(session, "invalidQuest")
    parafile <- parafile[key != ""]
    parafile <- parafile[, .(Invalids = length(interview__id)), by = .(key)]
    setorderv(parafile, "Invalids",
              order = ifelse(input$invalHighLow == "Most", -1, 1)
    )
    fast30 <- parafile[1:30]
    fast30 <- fast30[!is.na(Invalids)]
    material_spinner_hide(session, "invalidQuest")
    p <- plotlyBarH(
      data_in = fast30, x = "Invalids", y = "key",
      title = "Number of Invalid Responses",
      ordered_y = TRUE,
      x_title = "Number of Invalid Responses"
    )
    return(p)
  })


  #####################################################
  ##  7. Reactive tables and plots
  clickEvents1 <- reactiveValues(d = NULL, s = NULL)
  ##  7.1. EVENT FOR QUESTIONNAIRE
  observe({
    req(para_data_coll$para_data)
    s <- "WEEK"
    d <- event_data("plotly_click", source = s)
    if (!is.null(d)) {
      clickEvents1$d <- d
      clickEvents1$s <- s
    }
  })
  ## ii. Duration Plot
  observe({
    req(para_data_coll$para_data)
    s <- "Time per Interview (shortest/longest)"
    d <- event_data("plotly_click", source = s)
    if (!is.null(d)) {
      clickEvents1$d <- d
      clickEvents1$s <- s
    }
  })

  ## ii. Invalid Plot
  observe({
    req(para_data_coll$para_data)
    s <- "Number of Invalid Responses"
    d <- event_data("plotly_click", source = s)
    if (!is.null(d)) {
      clickEvents1$d <- d
      clickEvents1$s <- s
    }
  })

  ##  7.2. EVENT FOR INTERVIEWER
  clickEvents2 <- reactiveValues(d = NULL, s = NULL)
  ##  i. Aver Completion Plot
  observe({
    req(para_data_coll$para_data)
    s <- "Average Questionnaire Completion Time"
    d <- event_data("plotly_click", source = s)
    if (!is.null(d)) {
      clickEvents2$d <- d
      clickEvents2$s <- s
    }
  })
  ## ii. Pace Plot
  observe({
    req(para_data_coll$para_data)
    s <- "Average Pace"
    d <- event_data("plotly_click", source = s)
    if (!is.null(d)) {
      clickEvents2$d <- d
      clickEvents2$s <- s
    }
  })
  ## iii. Remove Plot
  observe({
    req(para_data_coll$para_data)
    s <- "Number of Response Removals"
    d <- event_data("plotly_click", source = s)
    if (!is.null(d)) {
      clickEvents2$d <- d
      clickEvents2$s <- s
    }
  })

  ## 7.2. Output
  ## i. QUESTIONNAIRE
  parafile_sub_quest <- reactive({
    ## GET the click events
    # d<-event_data("plotly_click", source = "WEEK")
    # d1<-event_data("plotly_click", source = "Time per Interview (shortest/longest)")
    d <- clickEvents1$d
    s <- clickEvents1$s
    parafile <- para_data_coll$para_data$AnswerSet
    shiny::validate(need(parafile, message = F))
    shiny::validate(need(s, message = F))

    # create url for review
    #keyURL<-paste0("<a href='https://recensaminte.insse.ro/Interview/Review/", interview__id, "' target='_blank'>", key, "</a>")


    if (input$dataLoad == "File" | input$dataLoad == "LocalFile") {
      # currently review is only supported when data is loaded from server
      shiny::showNotification(
        ui = "Currently review is only supported when data is loaded from server",
        type = "error", session = session,
        id = "no_link"
      )
      req(FALSE)
    } else if (input$dataLoad == "Server") {
      settings <- req(ADMIN$settings)
      keyURL<-function(server = settings[["suso.server"]], interview__id, key){
        #sprintf("<a href='%s/Interview/Review/%s' target='_blank'>%s</a>", server, interview__id, key)
        # build with httr
        furl<-httr::parse_url(server)
        furl$path<-list("Interview", "Review", interview__id)
        furl<-httr::build_url(furl)
        # parse key as label
        furl<-sprintf("<a href='%s' target='_blank'>%s</a>", furl, key)

      }
    }

    if (!is.null(d) & s == "WEEK") {
      #############################
      ##  1. Exclude Outliers
      parafile <- parafile[breaks == 0]
      parafile <- parafile[!is.na(resp_time)]
      parafile <- parafile[resp_time <= 120]
      ##  2. Transform Data
      ##  2.1. Get Question order based on median
      parafile <- parafile[!is.na(counter)]
      parafile_sub <- parafile[var == d$key]
      material_spinner_show(session, "timePlotTab")
      parafile_sub[, rid := ifelse(rid == "", 0, as.numeric(rid))]
      parafile_sub[, key2 := sprintf("%s-%02d", key, rid)]
      tab <- parafile_sub[, .(
        Usage = n_distinct(key2),
        AvTime = round(mean(resp_time, na.rm = T, trim = 0.05), 3),
        MainRole = modal(role)
      ),
      by = .(var, WEEK)
      ]
      d <- NULL
      return(tab)
    }
    if (!is.null(d) & s == "Time per Interview (shortest/longest)") {
      #############################
      ##  1. Exclude Outliers
      parafile <- parafile[breaks == 0]
      parafile <- parafile[!is.na(resp_time)]
      parafile <- parafile[resp_time <= 120]
      ##  2. Transform Data
      ##  2.1. Get Question order based on median
      parafile <- parafile[!is.na(counter)]
      parafile_sub <- parafile[key == d$key]
      # print(d)

      tab <- parafile_sub[, .(
        Questions = n_distinct(var),
        AvTimeQuestion = round(mean(resp_time, na.rm = T, trim = 0.05), 3),
        CompletionTime = mean(durationNOBREAK, na.rm = T),
        MainRole = modal(role),
        MainResponsible = modal(responsible)
      ),
      by = .(key, interview__id)
      ]
      tab[, key := keyURL(interview__id = interview__id, key = key)]
      tab[, interview__id := NULL]
      d1 <- NULL
      return(tab)
    }
    if (!is.null(d) & s == "Number of Invalid Responses") {
      #############################
      ##  1. Exclude Outliers
      ##  get answer inval
      parafile1 <- para_data_coll$para_data$QuestionDeclaredInvalid
      ##  clean main answer file
      parafile <- parafile[breaks == 0]
      parafile <- parafile[!is.na(resp_time)]
      parafile <- parafile[resp_time <= 120]
      ##  2. Transform Data
      ##  2.1. Get Question order based on median
      parafile <- parafile[!is.na(counter)]
      parafile_sub <- parafile[key == d$key]
      tab <- parafile_sub[, .(
        Questions = n_distinct(var),
        AvTimeQuestion = round(mean(resp_time, na.rm = T, trim = 0.05), 3),
        CompletionTime = mean(durationNOBREAK, na.rm = T),
        MainRole = modal(role),
        MainResponsible = modal(responsible)
      ),
      by = .(key, interview__id)
      ]
      tab[, key := keyURL]
      tab[, interview__id := NULL]
      d1 <- NULL
      return(tab)
    }
  })

  output$timePlotTab <- DT::renderDataTable({
    tab <- parafile_sub_quest()
    shiny::validate(need(tab, message = "Select Variable/Questionnaire first!"))

    tab <- DT::datatable(tab, smTab,
                         selection = "single", rownames = F, escape = F,
                         # colnames = c("Variable","Week","Usage","Average Time", "Main Role"),
                         style = "bootstrap"
    ) %>% infoTable()
    material_spinner_hide(session, "timePlotTab")
    return(tab)
  })

  output$timePlotPlot <- renderPlotly({
    d <- clickEvents1$d
    s <- clickEvents1$s
    parafile <- para_data_coll$para_data$AnswerSet
    shiny::validate(need(parafile, message = F))
    shiny::validate(need(s, message = F))
    if (!is.null(d) & s == "WEEK") {
      #############################
      ##  1. Exclude Outliers
      parafile <- parafile[breaks == 0]
      parafile <- parafile[!is.na(resp_time)]
      parafile <- parafile[resp_time <= 120]
      ##  2. Transform Data
      ##  2.1. Get Question order based on median
      parafile <- parafile[!is.na(counter)]
      parafile_sub <- parafile[var == d$key]
      parafile_sub[, surveyDay := .GRP, by = .(MONTH, mDAY)]
      material_spinner_show(session, "timePlotPlot")

      parafile_sub[, rid := ifelse(rid == "", 0, as.numeric(rid))]
      parafile_sub[, key2 := sprintf("%s-%02d", key, rid)]
      getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
      p <- plot_ly() %>%
        add_trace(
          data = parafile_sub,
          y = ~resp_time,
          x = ~surveyDay,
          name = "",
          type = "scatter",
          mode = "markers",
          text = ~key,
          hoverinfo = "text",
          showlegend = F,
          marker = list(color = rep("#009FDA", nrow(parafile_sub)))
        ) %>%
        layout(
          yaxis = list(title = "time"),
          xaxis = list(title = "S.Day")
        )
      d <- NULL
      material_spinner_hide(session, "timePlotPlot")
      return(p)
    }
  })
  ###################################################
  ## SUMMARY TABLES FOR REPORT
  ## 1. QUESTIONS
  qsumary <- reactive({
    parafile <- para_data_coll$para_data$AnswerSet
    shiny::validate(need(parafile, message = T))
    parafile <- parafile[breaks == 0]
    parafile <- parafile[!is.na(resp_time)]
    parafile <- parafile[resp_time <= 120]
    ##  2. Transform Data
    ##  2.1. Get Question order based on median
    parafile <- parafile[!is.na(counter)]
    qPos <- parafile[, .(counterMedian = median(counter, na.rm = T)), by = .(var)]
    setkeyv(qPos, "var")
    setkeyv(parafile, "var")
    parafile <- qPos[parafile, on = "var"]
    ## 2.2. Get slider inputs
    TL <- ifelse(length(unique(parafile$WEEK)) > 1, "WEEK", NULL)

    parafile <- parafile[, .(
      Av_ResponseTime = round(mean(resp_time, na.rm = T), 2),
      Av_Duration = round(mean(m_resp_time_varTRIM, na.rm = T), 2),
      N_obs = n_distinct(interview__id)
    ), by = .(counterMedian, var)]

    return((parafile))
  })
  ## 2. INTERVIEW
  isumary <- reactive({
    parafile <- para_data_coll$para_data$AnswerSet
    shiny::validate(need(parafile, message = T))
    parafile <- parafile[breaks == 0]
    parafile <- parafile[!is.na(resp_time)]
    parafile <- parafile[resp_time <= 120]
    ##  2. Transform Data
    ##  2.1. Get Question order based on median
    parafile <- parafile[!is.na(counter)]
    qPos <- parafile[, .(counterMedian = median(counter, na.rm = T)), by = .(var)]
    setkeyv(qPos, "var")
    setkeyv(parafile, "var")
    parafile <- qPos[parafile, on = "var"]
    ## 2.2. Get slider inputs
    TL <- ifelse(length(unique(parafile$WEEK)) > 1, "WEEK", NULL)

    parafile <- parafile[, .(
      mean_duration = round(mean(duration, na.rm = T), 2),
      mean_durationNOBREAK = round(mean(durationNOBREAK, na.rm = T), 2),
      startHour = first(startHour),
      mean_RespTime = round(mean(resp_time, na.rm = T), 2),
      N_obs = n_distinct(var)
    ), by = .(interview__id, responsible)]
    return((parafile))
  })
  ## 3. INTERVIEWER
  intsumary <- reactive({
    parafile <- isumary()
    shiny::validate(need(parafile, message = T))

    parafile <- parafile[, .(
      mean_duration = round((mean(mean_duration, na.rm = T)), 2),
      mean_durationNOBREAK = round(mean(mean_durationNOBREAK, na.rm = T), 2),
      startHour = first(startHour),
      mean_RespTime = round(mean(mean_RespTime, na.rm = T), 2),
      N_obs = n_distinct(interview__id)
    ), by = .(responsible)]
    return((parafile))
  })
  ##################################################################################################
  ##                                            REPORTS FOR DOWNLOAD
  ## REPORT FOR QUESTIONNAIRE (P1)
  SUM_NAT <- reactiveVal(NULL)
  SUM_COUNTY <- reactiveVal(NULL)
  SUM_QUEST <- reactiveVal(NULL)
  report_content_q <- reactive({
    fpwww<-system.file("www", package = "susoparaviewer")
    ## CREATE POP SEGMENT LIST
    pop_segment <- list()
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


    #################################################
    ## TITLE
    qTitle <- paste("Survey Solutions Paradata Report Questionnaire")
    ## Fonts title
    fp_title <- fp_text(color = "#546e7a", font.size = 20, bold = T)
    fp_title_sty <- fp_par(
      text.align = "center", padding.bottom = 1,
      border.bottom = fp_border(color = "black")
    )
    fp_stitle <- fp_text(color = "#546e7a", font.size = 14, bold = F)
    fp_stitle_sty <- fp_par(text.align = "center", padding.bottom = 0)
    ##
    pop_segment$doc_title <- block_list(
      fpar(ftext(qTitle, prop = fp_title),
           run_linebreak(), run_linebreak(),
           fp_p = fp_par(text.align = "center")
      ),
      fpar(ftext("Survey Solutions Quality Report Tools", prop = fp_stitle), fp_p = fp_stitle_sty),
      fpar(ftext(as.character(Sys.Date()), prop = fp_stitle),
           run_linebreak(), run_linebreak(),
           run_linebreak(), run_linebreak(),
           fp_p = fp_stitle_sty
      ),
      fpar(
        external_img(src = file.path(fpwww, "suso_wb.png"), height = 1.06 * 2, width = 1.39 * 2),
        fp_p = fp_par(text.align = "center", padding.top = 5)
      )
    )
    ####################
    ## SECTION TITLE
    pop_segment$sec_title <- list(
      sec1 = "Total Summary",
      sec2 = "Summary of Individual Questions",
      sec3 = "Summary of Individual Interviews"
    )

    ##################
    ## SECTION Text
    pop_segment$sec_para <- list(
      sec1 = list(
        para1 = freestyler(""),
        para2 = freestyler("")
      ),
      sec2 = list(
        para1 = freestyler(""),
        para2 = freestyler("")
      ),
      sec3 = list(para1 = freestyler(""))
    )
    #################################################
    ## TABLE
    qsumaryall <- qsumary()[, tot := "Overall"][, .(
      Av_ResponseTime = mean(Av_ResponseTime),
      Av_Duration = mean(Av_ResponseTime),
      N_questions = n_distinct(var)
    ), by = .(tot)]
    setnames(qsumaryall, "tot", ".")
    pop_segment$sec_table <- list(
      sec1 = list(para1 = as.data.frame(qsumaryall)),
      sec2 = list(para1 = as.data.frame(qsumary())),
      sec3 = list(para1 = as.data.frame(isumary()))
    )

    return(pop_segment)
  })
  #################################################
  ## DOWNLOAD QUESTIONNAIRE REPORT
  callModule(dwl_reportSRV,
             "dwl_q_report",
             rname = "Survey Solutions",
             content = report_content_q,
             creator = paste0("Survey Solutions Paradata Viewer")
  )

  ## iI. INTERVIEWER
  parafile_sub_int <- reactive({
    ## GET the click events
    # d<-event_data("plotly_click", source = "WEEK")
    # d1<-event_data("plotly_click", source = "Time per Interview (shortest/longest)")
    d <- clickEvents2$d
    s <- clickEvents2$s
    parafile <- para_data_coll$para_data$AnswerSet
    shiny::validate(need(parafile, message = F))
    shiny::validate(need(s, message = F))
    # print(d)
    # print(s)
    if (!is.null(d) & s == "Average Questionnaire Completion Time") {
      #############################
      ##  1. Exclude Outliers
      parafile <- parafile[breaks == 0]
      parafile <- parafile[!is.na(resp_time)]
      parafile <- parafile[resp_time <= 120]
      ##  2. Transform Data
      ##  2.1. Get Question order based on median
      parafile <- parafile[!is.na(counter)]
      parafile_sub <- parafile[responsible == d$key]
      parafile_sub[, rid := as.numeric(rid)]
      material_spinner_show(session, "intPlotTab")
      tab <- parafile_sub[, .(
        Av_DurationQuestionnaire = round(mean(durationNOBREAK, na.rm = T), 2),
        NumberInterviews = n_distinct(key),
        Fast = sum(m_diff_dev < 0),
        Slow = sum(m_diff_dev >= 0),
        MaxRoster = max(rid, na.rm = T)
      ), by = .(responsible)]
      d <- NULL
      return(tab)
    }
    if (!is.null(d) & s == "Average Pace") {
      #############################
      ##  1. Exclude Outliers
      parafile <- parafile[breaks == 0]
      parafile <- parafile[!is.na(resp_time)]
      parafile <- parafile[resp_time <= 120]
      ##  2. Transform Data
      ##  2.1. Get Question order based on median
      parafile <- parafile[!is.na(counter)]
      parafile_sub <- parafile[responsible == d$key]
      parafile_sub[, rid := as.numeric(rid)]
      tab <- parafile_sub[, .(
        Av_DurationQuestionnaire = round(mean(durationNOBREAK, na.rm = T), 2),
        NumberInterviews = n_distinct(key),
        Fast = sum(m_diff_dev < 0),
        Slow = sum(m_diff_dev >= 0),
        MaxRoster = max(rid, na.rm = T)
      ), by = .(responsible)]
      d <- NULL
      return(tab)
    }
    if (!is.null(d) & s == "Number of Response Removals") {
      #############################
      ##  1. Exclude Outliers
      ##  clean main answer file
      parafile <- parafile[breaks == 0]
      parafile <- parafile[!is.na(resp_time)]
      parafile <- parafile[resp_time <= 120]
      ##  2. Transform Data
      ##  2.1. Get Question order based on median
      parafile <- parafile[!is.na(counter)]
      parafile_sub <- parafile[responsible == d$key]
      parafile_sub[, rid := as.numeric(rid)]
      tab <- parafile_sub[, .(
        Av_DurationQuestionnaire = round(mean(durationNOBREAK, na.rm = T), 2),
        NumberInterviews = n_distinct(key),
        Fast = sum(m_diff_dev < 0),
        Slow = sum(m_diff_dev >= 0),
        MaxRoster = max(rid, na.rm = T)
      ), by = .(responsible)]
      d <- NULL
      return(tab)
    }
  })

  output$intPlotTab <- DT::renderDataTable({
    tab <- parafile_sub_int()
    shiny::validate(need(tab, message = "Select Variable/Questionnaire first!"))

    tab <- DT::datatable(tab, smTab,
                         selection = "single", rownames = F,
                         # colnames = c("Variable","Week","Usage","Average Time", "Main Role"),
                         style = "bootstrap"
    ) %>% infoTable()
    material_spinner_hide(session, "intPlotTab")
    return(tab)
  })
  #################################################
  ## REPORT FOR INTERVIEWER
  report_content_int <- reactive({
    fpwww<-system.file("www", package = "susoparaviewer")
    ## main list
    pop_segment <- list()
    ## helper functions
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

    ## TITLE
    qTitle <- paste("Survey Solutions Paradata Report Interviewer")

    fp_title <- fp_text(color = "#546e7a", font.size = 20, bold = T)
    fp_title_sty <- fp_par(
      text.align = "center", padding.bottom = 1,
      border.bottom = fp_border(color = "black")
    )
    fp_stitle <- fp_text(color = "#546e7a", font.size = 14, bold = F)
    fp_stitle_sty <- fp_par(text.align = "center", padding.bottom = 0)
    ##
    pop_segment$doc_title <- block_list(
      fpar(ftext(qTitle, prop = fp_title),
           run_linebreak(), run_linebreak(),
           fp_p = fp_par(text.align = "center")
      ),
      fpar(ftext("Survey Solutions Quality Report Tools", prop = fp_stitle), fp_p = fp_stitle_sty),
      fpar(ftext(as.character(Sys.Date()), prop = fp_stitle),
           run_linebreak(), run_linebreak(),
           run_linebreak(), run_linebreak(),
           fp_p = fp_stitle_sty
      ),
      fpar(
        external_img(src = file.path(fpwww, "suso_wb.png"), height = 1.06 * 2, width = 1.39 * 2),
        fp_p = fp_par(text.align = "center", padding.top = 5)
      )
    )

    ## SECTION TITLES
    pop_segment$sec_title <- list(
      sec1 = "Total Summary",
      sec2 = "Interviewer Summary"
    )

    ##################
    ## SECTION
    pop_segment$sec_para <- list(
      sec1 = list(
        para1 = freestyler(""),
        para2 = freestyler("")
      ),
      sec2 = list(
        para1 = freestyler(""),
        para2 = freestyler("")
      ),
      sec3 = list(para1 = freestyler(""))
    )
    #################################################
    ## TABLE
    intsumaryall <- intsumary()[, tot := "Overall"][, .(
      mean_duration = round((mean(mean_duration, na.rm = T)), 2),
      mean_durationNOBREAK = round(mean(mean_durationNOBREAK, na.rm = T), 2),
      mean_startHour = mean(startHour),
      mean_RespTime = round(mean(mean_RespTime, na.rm = T), 2),
      N_obs = n_distinct(responsible)
    ), by = .(tot)]
    setnames(intsumaryall, "tot", ".")
    pop_segment$sec_table <- list(
      sec1 = list(para1 = as.data.frame(intsumaryall)),
      sec2 = list(para1 = as.data.frame(intsumary()))
    )

    return(pop_segment)
  })

  ## DOWNLOAD QUESTIONNAIRE REPORT
  callModule(dwl_reportSRV,
             "dwl_int_report",
             rname = "Survey Solutions Paradata Viewer",
             content = report_content_int,
             creator = paste0("Survey Solutions Paradata Viewer")
  )

  ###################################################################################################
  ##  LEAFLET MAP
  ##  1. Prepare Map
  ADMSHP <- reactive({
    ##  2.1 Admin Boundaries Shape and Aggregation of continous variable
    shiny::validate(need(input$mapData, message = F))
    material_spinner_show(session, "admMap")

    # gps_file<-st_as_sf(gps_file, coords = c("long", "lat"), crs = 4326)
    ##  a) Completion Time
    if (input$mapData == "Time") {
      gps_file <- para_data_coll$para_data$AnswerSet
      # gps_file<-gps_file[!is.na(lat)&!is.na(long),]
      shiny::validate(need(nrow(gps_file) > 0, message = F))

      shiny::validate(need("lat" %in% names(gps_file), message = " No geo-referenced data avaialable!"))
      gps_file <- gps_file[, .(
        durationNOBREAK = mean(durationNOBREAK, na.rm = T),
        lat = mean(lat, na.rm = T),
        long = mean(long, na.rm = T)
      ), by = .(key)]
      gps_file <- gps_file[!is.na(lat) & !is.na(long), ]
      gps_file <- gps_file[!is.na(durationNOBREAK), ]
      gps_file <- st_as_sf(gps_file, coords = c("long", "lat"), crs = 4326)
      gps_file <- gps_file %>% st_transform(31700)

      gps_file_ras <- st_rasterize(
        gps_file,
        template = st_as_stars(
          st_bbox(gps_file),
          values = NA_real_,
          dx = 100, dy = 100
        )
      )
      gps_file_ras_poly <- st_as_sf(gps_file_ras[1], as_points = FALSE, merge = FALSE)

      # admShp<-GADM.getData(country = "ROU", sp.Library = "sf", level = 2, path = "./data/gadm/")

      admShp <- st_transform(gps_file_ras_poly, 4326)
      ## b) Answer Removed
    } else if (input$mapData == "Removals") {
      gps_file <- para_data_coll$para_data$AnswerRemoved
      shiny::validate(need("lat" %in% names(gps_file), message = "No geo-referenced data avaialable!"))
      gps_file <- gps_file[, .(
        Removals = .N,
        lat = mean(lat, na.rm = T),
        long = mean(long, na.rm = T)
      ), by = .(key)]
      gps_file <- gps_file[!is.na(lat) & !is.na(long), ]
      gps_file <- st_as_sf(gps_file, coords = c("long", "lat"), crs = 4326)
      gps_file <- gps_file %>% st_transform(31700)

      gps_file_ras <- st_rasterize(
        gps_file,
        template = st_as_stars(
          st_bbox(gps_file),
          values = NA_real_,
          dx = 100, dy = 100
        )
      )
      gps_file_ras_poly <- st_as_sf(gps_file_ras[1], as_points = FALSE, merge = FALSE)

      # admShp<-GADM.getData(country = "ROU", sp.Library = "sf", level = 2, path = "./data/gadm/")

      admShp <- st_transform(gps_file_ras_poly, 4326)
      ## c) Question Invalid
    } else if (input$mapData == "Invalids") {
      gps_file <- para_data_coll$para_data$QuestionDeclaredInvalid
      shiny::validate(need("lat" %in% names(gps_file), message = " No geo-referenced data avaialable!"))
      gps_fileVAR <- gps_file[, .(
        Invalids = .N,
        lat = mean(lat, na.rm = T),
        long = mean(long, na.rm = T)
      ), by = .(key, var)]
      gps_file <- gps_file[, .(
        Invalids = .N,
        lat = mean(lat, na.rm = T),
        long = mean(long, na.rm = T)
      ), by = .(key)]
      gps_file <- gps_file[!is.na(lat) & !is.na(long), ]
      gps_file <- st_as_sf(gps_file, coords = c("long", "lat"), crs = 4326)
      gps_file <- gps_file %>% st_transform(31700)

      gps_file_ras <- st_rasterize(
        gps_file,
        template = st_as_stars(
          st_bbox(gps_file),
          values = NA_real_,
          dx = 100, dy = 100
        )
      )
      gps_file_ras_poly <- st_as_sf(gps_file_ras[1], as_points = FALSE, merge = FALSE)

      # admShp<-GADM.getData(country = "ROU", sp.Library = "sf", level = 2, path = "./data/gadm/")

      admShp <- st_transform(gps_file_ras_poly, 4326)
    }
    material_spinner_hide(session, "admMap")
    admShp$Aggregate <- 1:nrow(admShp)
    return(admShp)
  })
  ######################################################
  ##  2. Creat Map

  #################################################################
  ## 0. Build UI
  output$MAP_UI <- shiny::renderUI({
    if (getOption("mapwidget.option") == "mapdeck") {
      tagList(
        mapModuleUI("baseMap", height = "90vh")
      )
    } else if (getOption("mapwidget.option") == "leaflet") {
      tagList(
        mapUI("baseMap_leaf", height = "90vh")
      )
    }
  })
  ## 1. BASE MAP
  observe(
    {
      m <- req(ADMSHP())

      if (getOption("mapwidget.option") == "mapdeck") {
        shiny::validate(need(m, message = F))
        m$tempgr <- 1L
        mapModuleSvr(
          id = "baseMap",
          updateMap = reactive({
            m
          }),
          z_var = reactive(names(m)[1]),
          updateGroup = reactive("tempgr"),
          polyId = reactive(NULL),
          transitions = list(
            polygon = 2000,
            fill_colour = 2000,
            stroke_width = 2000,
            elevation = 2000
          )
        )
      } else if (getOption("mapwidget.option") == "leaflet") {
        mapServer("baseMap_leaf",
                  updateMap = reactive({
                    m
                  }),
                  z_var = reactive(names(m)[1]),
                  updateGroup = reactive("tempgr")
        )
      }
    },
    autoDestroy = F
  )



  ##  3. Store Click Map
  mapTabData <- eventReactive(input$admMap_shape_click, {
    p <- input$admMap_shape_click
    admShp <- ADMSHP()
    shiny::validate(need(admShp, message = "No Data Loaded!"))
    material_spinner_show(session, "mapTab")
    shiny::validate(need(p, message = F))
    ##################################
    ##  create zonal table
    admShpSel <- admShp[admShp$NAME_2 == p$id, ]
    tab <- data.table(
      Variable = c("Area Name", "Area Type", input$mapData, "Number of Interviews"),
      Value = c(admShpSel$NAME_2, admShpSel$TYPE_2, admShpSel$Aggregate, admShpSel$Count)
    )
    return(tab)
  })

  output$mapTab <- DT::renderDataTable({
    tab <- mapTabData()
    shiny::validate(need(tab, message = "Select Area First!"))

    tab <- DT::datatable(tab, smTab,
                         selection = "single", rownames = F,
                         colnames = c("", ""),
                         style = "bootstrap"
    ) %>% infoTable()
    material_spinner_hide(session, "mapTab")
    return(tab)
  })

  report_content_m <- reactive({
    paste("NOT Yet")
  })

  ## DOWNLOAD QUESTIONNAIRE REPORT
  callModule(dwl_reportSRV,
             "dwl_m_report",
             rname = "Survey Solutions Paradata Viewer",
             content = report_content_m,
             creator = paste0("Survey Solutions Paradata Viewer")
  )

  ###################################################################################################
  ##  DATA VIEW and DOWNLOAD
  ##  1. Data Table Viewer
  output$viewData <- DT::renderDataTable({
    allFiles <- para_data_coll$para_data

    fileSel <- input$file_select_view
    shiny::validate(
      need(allFiles, message = "Upload Data First!"),
      need(fileSel, message = F),
      need(fileSel != "(Up)Load Data First!", message = F)
    )
    material_spinner_show(session, "viewData")
    fromDate <- input$dateFrom
    toDate <- input$dateTo
    fromDate <- as.IDate(as.Date(fromDate, "%d %B, %Y"))
    toDate <- as.IDate(as.Date(toDate, "%d %B, %Y"))
    allFiles <- allFiles[fileSel][[1]]
    allFiles <- allFiles[date >= fromDate & date <= toDate]
    ## print(allFiles)
    suppressWarnings(
      allFiles[, c("date", "time", "wDAY", "mDAY", "MONTH", "lat", "long", "interview__id", "var_resp", "response1", "response2", "response3", "response4", "response5") := NULL]
    )
    maxCol <- min(length(allFiles), 10)
    allFiles <- allFiles[, c(1:maxCol), with = F]
    ## print(str(allFiles))
    tab <- datatable(allFiles, rownames = F)
    material_spinner_hide(session, "viewData")
    return(tab)
  })
  ######################################################
  ##  2. DOWNLOAD paradata
  ##  2.1. INITIATE m action
  fs1 <- reactiveVal()
  observeEvent(input$downloadData_ini, {
    fs <- paste("Paradata-", Sys.time(), ".csv", sep = "")
    fs.zip <- paste("Paradata-", Sys.time(), ".zip", sep = "")
    frame_data <- para_data_coll$para_data
    ## Clear TMP dir
    unlink(paste0(tempdir(), "/*"))

    temp.dir <- tempdir()
    shiny::validate(need(frame_data, message = F))

    withProgress(
      message = paste("Preparing data for download"),
      value = 0,
      {
        wdOld <- getwd()
        setwd(temp.dir)
        ## i. get all files
        allFiles <- para_data_coll$para_data

        shiny::validate(need(allFiles, message = F))
        all_names <- names(allFiles)
        ## iii. loop to write the files directly in wd
        ##      (in this case first csv to avoide UTF8 problems)
        ## withprogress
        fs <- character(0)
        for (i in all_names) {
          finalFile <- allFiles[[i]]
          path <- file.path(paste0(i, ".csv"))
          if (nrow(finalFile) == 0) next()
          fs <- c(fs, path)
          fwrite(finalFile, path)
        }
        zip(zipfile = fs.zip, files = fs)
        fs1(fs.zip)
      }
    )
    # if(file.exists(fs.zip)) {runjs("$('#downloadData')[1].click();")}
    toggleElement("downloadData", condition = file.exists(fs.zip))
    setwd(wdOld)
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Paradata", Sys.time(), ".zip", sep = "")
    },
    content = function(file) {
      withProgress(
        message = paste("Downloading Data..."),
        value = 0,
        {
          wdOld <- getwd()
          setwd(tempdir())
          fs.zip <- fs1()
          shiny::validate(need(file.exists(fs.zip), message = F))
          on.exit(setwd(wdOld))
          file.copy(fs.zip, file)
        }
      )
    },
    contentType = "application/zip"
  )


  #####################################################################
  #####################################################################
  ##    ADD OWNTRACKS
  #####################################################################
  #####################################################################
  counter <- reactiveValues()

  ######################################################
  ##  1. Update Server in admin settings
  observeEvent(input$trackMode, {
    if (input$trackMode) {
      updateTextInput(session,
                      "trackingIP",
                      "Please provide server address and port (i.e. IP:PORT)",
                      value = "34.224.75.201:1883"
      )
    } else {
      updateTextInput(session,
                      "trackingIP",
                      "Please provide server address and port (i.e. IP:PORT)",
                      value = "34.224.75.201:80"
      )
    }
  })
  # ######################################################
  # ##  2. LOAD tracks
  # observeEvent(input$loadtrack, {
  #   shiny::validate(need(input$acc, message = F))
  #   a<-readOwntracksDB(num5 = input$acc, usersOnly = T)
  #   shiny::validate(need(a, message = F),
  #                   need(input$pass!="", message = F))
  #   if (input$pass=="RomaniaLFS2018") a<-a[a$user!="tester"&a$user!="tester1",]
  #   ch<-levels(a$user)
  #   ch<-setNames(object = ch, ch)
  #   update_material_dropdown(session,
  #                            input_id = "team",
  #                            choices = ch,
  #                            value = ch[1])
  # }, suspended = F)
  #
  ######################################################
  ##  3. RELOAD tracks
  trackSF <- reactive({
    trackRefresh <- isolate(input$trackRefresh * 1000)
    invalidateLater(trackRefresh, session = NULL)

    if (input$loadtrack == T) {
      shiny::validate(
        need(input$acc, message = F),
        need(input$team != "No Data Loaded!", message = F)
      )
      #a <- readOwntracksDB(num5 = input$acc, usersOnly = F, USER = input$team)
      return(a)
      Sys.sleep(0.1)
    }
  })

  ######################################################
  ##  4. SHAPE file for tracking
  tracksShape <- reactive({
    counter <- counter$counter
    counter <- ifelse(!is.null(counter), counter, 0)
    shiny::validate(need(input$acc >= 5, message = F))
    a <- trackSF()
    shiny::validate(
      need(a, message = F),
      need(input$team != "No Data Loaded!", message = F)
    )
    c_1 <- sum(a$n)
    if (input$pass == "RomaniaLFS2018") a <- a[a$user != "tester" | a$user != "tester1", ]
    a <- a[a$user == input$team, ]
    if (counter != c_1) {
      # print(a)
      a <- a[!st_is_empty(a), ]
      # print(a)
      a <- as(a, "Spatial")
      a$distKM <- SpatialLinesLengths(a, longlat = T)
      a$id <- rownames(a@data)
      adt <- data.table(tidy(a))
      a <- merge(adt, a@data, by = "id")
      trackShapeArea <- getGADMbyCoord(a, aggregation.var = "timeDiff")
      return(trackShapeArea)
    }
  })


  #############################################
  ##  5. OWNTRACKS LEAFLET MAP
  output$trackMap <- renderLeaflet({
    if (input$loadtrack == T) {
      admShp <- tracksShape()
      gps_file <- trackSF()
      shiny::validate(need(admShp, message = "No Data Loaded!"))
      admShp <- st_transform(admShp, 4326)
      ##  ABSURD muss mit sf gehen
      gps_file <- as(gps_file, "Spatial")
      gps_file$distKM <- round(SpatialLinesLengths(gps_file, longlat = T), 3)
      gps_file <- gps_file[gps_file$user == input$team, ]
      ##  1. Preparation
      ##  1.1 color palette
      pal <- colorNumeric(palette = "magma", domain = admShp$Aggregate)
      pal1 <- colorFactor(palette = "viridis", domain = gps_file$tid)
      ##  1.2. Popup
      ##  a) shape
      popup <- paste(
        sep = "<br/>",
        "<b>Admin Area Type:</b>",
        admShp$TYPE_2,
        "<b>Admin Area Name:</b>",
        admShp$NAME_2,
        "<b>Average time in area (in minutes):</b>",
        admShp$Aggregate,
        "<b>Total Number of Observations:</b>",
        admShp$Count
      )
      ##  b) points
      popup1 <- paste(
        sep = "<br/>",
        "<b>ID:</b>",
        gps_file$tid,
        "<b>Completed Distance (KM):</b>",
        gps_file$distKM,
        "<b>Date:</b>",
        gps_file$DATE1
      )
      baseMap <- leaflet() %>%
        addProviderTiles("Esri.WorldImagery",
                         layerId = 1,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addPolygons(
          data = admShp,
          weight = 1,
          color = ~ pal(Aggregate),
          fillOpacity = 0.5,
          opacity = 1,
          layerId = 2,
          popup = ~ (popup),
          popupOptions = popupOptions(closeOnClick = T)
        ) %>%
        addPolylines(
          data = gps_file, color = ~ pal1(tid),
          popup = ~ (popup1)
        ) %>%
        addLegend(
          data = gps_file, "bottomright", pal = pal1, values = ~tid,
          title = "Interviewer",
          # labFormat = labelFormat(suffix = " Min"),
          opacity = 1
        )
      return(baseMap)
    }
  })


  ######################################################################
}
