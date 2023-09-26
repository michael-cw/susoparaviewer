

paraTransformation<-function(paradata_files = NULL, timeDiffMax = 120,
                             inShinyServer=FALSE,
                             multiCore = 14,
                             onlyActiveEvents = FALSE,
                             allResponses = TRUE,
                             gpsVarName = NA,
                             verbose = T,
                             showProgress = F,
                             L0 = NA) {
  ###################
  ## para transformation for manual load
  ########################### FILE ############################################
  para_data<-list()
  ## helpers
  checkNum<-function(x) {
    # if (!is.numeric(x)) stop("Number of cores needs to be NULL or an integer!")
  }

  ## TRANSFORMATIONS
  ## A add rid if it doesnt exist
  if (length(grep("rid", names(paradata_files)))==0) paradata_files[,rid:=0]

  ##################################################################
  ##  2.1. Get Start/End date
  #fromDate<-as.character(min(paradata_files$date, na.rm = T), "%d %B, %Y")
  #toDate<-as.character(max(paradata_files$date, na.rm = T), "%d %B, %Y")

  ##################################################################
  ##  2.2. GET ALL ACTION COUNTS
  actionDistr<-paradata_files[,.(count=.N), by=.(action)]
  setorderv(actionDistr, "count", order = -1)
  ##################################################################
  ##  2.3. GET ALL RESPONSIBLE COUNTS
  userDistr<-paradata_files[,.(count=.N), by=.(responsible)]
  setorderv(userDistr, "count", order = -1)
  ##################################################################
  ##  2.4. GET ALL ROLE COUNTS
  roleDistr<-paradata_files[,.(count=.N), by=.(role)]
  setorderv(roleDistr, "count", order = -1)

  ##  2.5. Extract questionnaire ID and Key
  KeyAssigned<-paradata_files[action=="KeyAssigned"][,c("responsible", "role", "var_resp", "rid"):=NULL]
  setnames(KeyAssigned, "var", "key")
  KeyAssigned<-droplevels(KeyAssigned)
  paradata_files<-paradata_files[action!="KeyAssigned"]
  paradata_files<-droplevels(paradata_files)
  KeyAssigned<-KeyAssigned[,.SD[1], by=.(interview__id)]
  KeyAssigned_merge<-KeyAssigned[ ,.(interview__id, key)]
  setkeyv(KeyAssigned, "interview__id")
  para_data$KeyAssigned<-KeyAssigned
  ##  2.6. Comments
  CommentSet<-paradata_files[action=="CommentSet"]
  if(nrow(CommentSet)>0){
    setnames(CommentSet, "var_resp", "comment")
    CommentSet<-droplevels(CommentSet)
    paradata_files<-paradata_files[action!="CommentSet"]
    paradata_files[,action:=droplevels(action)]
    para_data$CommentSet<-CommentSet
  }
  ## 2.7 Completed
  Completed<-paradata_files[action=="Completed"][,c("responsible", "role" ,"var_resp", "rid"):=NULL]
  if(nrow(Completed)>0){
    setnames(Completed, "var", "comment")
    Completed<-droplevels(Completed)
    paradata_files<-paradata_files[action!="Completed"]
    paradata_files[,action:=droplevels(action)]
    para_data$Completed<-Completed
  }
  ##  2.8. AnswerSet
  para1_answer<-paradata_files[action=="AnswerSet"|action=="Paused"]
  para1_answer[,action:=droplevels(action)]

  ##  3. Time Difference (SORT by counter)
  calcTimeDiff<-function(DTfile, by=c("interview__id","DAY", "MONTH"), tdm = NULL){
    DTfile<-copy(DTfile)
    setorderv(DTfile, c("interview__id","date", "time", "counter"))
    #DTfile<-DTfile[,.SD[.N>20], by=.(interview__id)]
    DTfile[,resp_time:=as.integer(0)][ ,resp_time:=ifelse((data.table::shift(date, type = "lag")==date & action!="Paused"),
                                                          time-data.table::shift(time, type = "lag"), NA), by=.(interview__id)][]
    DTfile[, breaks:=ifelse(resp_time>tdm & !is.na(resp_time),1,0), by=.(interview__id)]
    return(DTfile)
  }


  ##  3.1. Function (use shift/lead, and check lead date is the same)
  cat("\nCalculating Response Timings.\n")
  para1_answer<-calcTimeDiff(para1_answer, tdm = timeDiffMax)
  ##  3.2. Other calculations
  para1_answer<-para1_answer[!is.na(breaks)]
  para1_answer[,duration:=round((sum(resp_time, na.rm = T))/60, 2), by = .(interview__id)]
  para1_answer[breaks==0,durationNOBREAK:=round((sum(resp_time, na.rm = T))/60, 2), by = .(interview__id)]
  para1_answer[,m_resp_time_varTRIM:=(mean(resp_time, na.rm = T, trim = 0.05)), by = .(var)]
  para1_answer[,m_resp_time_var:=(mean(resp_time, na.rm = T)), by = .(var)]
  para1_answer[breaks==0,m_diff_dev:=resp_time-m_resp_time_varTRIM]
  para1_answer[,start:=min(time, na.rm = T), by=.(interview__id)]
  para1_answer[,startHour:=min(data.table::hour(time), na.rm = T), by=.(interview__id)]
  para1_answer[,role:=droplevels(role)][,responsible:=droplevels(responsible)]
  para1_answer[,var:=as.factor(var)]
  para1_answer_merge<-para1_answer[,.SD[1], by=.(interview__id, role)]
  para1_answer_merge<-para1_answer_merge[ ,.(interview__id, responsible, role)]

  if(!is.na(L0)) {
    para1_answer[,duration_sec:=round((sum(resp_time, na.rm = T))/60, 2),
                 by = .(L0, interview__id)]
    para1_answer[breaks==0,durationNOBREAK_sec:=round((sum(resp_time, na.rm = T))/60, 2),
                 by = .(L0, interview__id)]

  }

  ##  2. GPS extract -->if no name, try identification through grepl
  varNames<-levels(para1_answer$var)
  if(is.na(gpsVarName)) {
    gpsVarMain<-varNames[grepl("gps", tolower(varNames))]
  } else {
    stopifnot(is.character(gpsVarName), gpsVarName %in% varNames)
    gpsVarMain<-gpsVarName
  }

  ## create gps file when exists
  if (length(gpsVarMain)>0) {
    ## Select first gps variable
    cat("\nExtracting GPS variable.\n")
    gpsVar<-gpsVarMain[1]
    gps_file<-para1_answer[var==gpsVar]
    if(nrow(gps_file)==0) stop(cat("No GPS values found with: ", gpsVarName ))
    if (!allResponses) {
      gp<-gps_file[,tstrsplit(response, ",", fixed=T, fill = "<NA>", names = TRUE)][]
      gps_file<-cbind(gps_file, gp)
      setnames(gps_file, c("V1", "V2"), c("response1", "response2"))
    }

    gps_file<-gps_file[, .(interview__id, responsible, time, var_resp, var,
                           date, durationNOBREAK, response1, response2)]
    gps_file<-gps_file[,c("long"):=tstrsplit(response2, "[", fixed=T ,keep=c(1))][]
    gps_file[,lat:=as.numeric(as.character(response1))]
    gps_file[,long:=as.numeric(as.character(long))]
    gpsSelect<-sum(!is.na(gps_file$lat))
    ## If empty iterate over next/only if length>1/until length==k
    k<-2
    while(gpsSelect>=0 & gpsSelect<=nrow(gps_file) & length(gpsVarMain) >1 & length(gpsVarMain) != k) {
      gpsVar<-gpsVarMain[k]
      gps_file<-para1_answer[var==gpsVar]
      if (!allResponses) {
        gp<-gps_file[,tstrsplit(response, ",", fixed=T, fill = "<NA>", names = TRUE)][]
        gps_file<-cbind(gps_file, gp)
        setnames(gps_file, c("V1", "V2"), c("response1", "response2"))
      }
      gps_file<-gps_file[, .(interview__id, responsible, time, var_resp,
                             date, durationNOBREAK, response1, response2)]
      gps_file<-gps_file[,c("long"):=tstrsplit(response2, "[", fixed=T ,keep=c(1))][]
      gps_file[,lat:=as.numeric(as.character(response1))]
      gps_file[,long:=as.numeric(as.character(long))]
      k<-k+1
      gpsSelect<-sum(!is.na(gps_file$lat))
    }
    ##  For merge with EVENT data
    gps_file_merge<-gps_file[,.(interview__id, lat, long)]
    gps_file_merge<-gps_file_merge[,.SD[1], by=.(interview__id)]
    setkeyv(gps_file_merge, "interview__id")
  }
  ##  Subset with function, key and lapply
  ## loop over levels of action with LAPPLY
  ## a<-lapply(levels(CHECK$action), FUN = subsetDataTableAction, CHECK)
  ##  not used for now
  subsetDataTableAction<-function(dt, x) {
    setkeyv(x, "action")
    file<-x[dt]
    return(file)
  }

  if (is.null(multiCore)) {
    cat("Processing: ")
    cat("\n\tAnswerSet\n")
    ##  ACTIVE EVENTS
    ##  2.8 Answer Set
    AnswerSet<-para1_answer
    AnswerSet<-AnswerSet[!is.na(interview__id)]
    setkeyv(AnswerSet, "interview__id")
    if(exists("gps_file_merge"))  AnswerSet<-gps_file_merge[AnswerSet, on="interview__id"]
    AnswerSet<-KeyAssigned_merge[AnswerSet, on="interview__id"]
    para_data$AnswerSet<-AnswerSet
    ##  2.9. Answer Removed (COUNT the number of Removed answer by questionnaire)
    cat("\n\tAnswerRemoved\n")
    AnswerRemoved<-paradata_files[action=="AnswerRemoved"]
    AnswerRemoved<-AnswerRemoved[!is.na(interview__id)]
    AnswerRemoved[, count:=length(counter), by=interview__id]
    AnswerRemoved[,c("responsible", "role"):=NULL]
    #AnswerRemoved<-droplevels(AnswerRemoved)
    AnswerRemoved<-merge(AnswerRemoved, para1_answer_merge, by="interview__id", allow.cartesian=T)
    setkeyv(AnswerRemoved, "interview__id")
    if(exists("gps_file_merge")) AnswerRemoved<-gps_file_merge[AnswerRemoved, on="interview__id"]
    AnswerRemoved<-KeyAssigned_merge[AnswerRemoved, on="interview__id"]
    para_data$AnswerRemoved<-AnswerRemoved
    ##  2.10. Approved
    cat("\n\tApproveByHeadquarter\n")
    ApproveByHeadquarter<-paradata_files[action=="ApproveByHeadquarter"]
    ApproveByHeadquarter<-droplevels(ApproveByHeadquarter)
    ApproveBySupervisor<-paradata_files[action=="ApproveBySupervisor"]
    ApproveBySupervisor<-droplevels(ApproveBySupervisor)
    ##  2.13 Restarted
    cat("\n\tRestarted\n")
    Restarted<-paradata_files[action=="Restarted"]
    Restarted<-Restarted[!is.na(interview__id),]
    Restarted[, count:=length(counter), by=interview__id]
    setkeyv(Restarted, "interview__id")
    if(exists("gps_file_merge")) Restarted<-gps_file_merge[Restarted, on="interview__id"]
    Restarted<-KeyAssigned_merge[Restarted, on="interview__id"]
    para_data$Restarted<-Restarted
    ##  2.14. Rejected
    cat("\n\tReject\n")
    Reject<-paradata_files[action=="RejectedBySupervisor"|action=="RejectedByHeadquarter"][,c("var_resp", "rid"):=NULL]
    setnames(Reject, "var", "comment")
    Reject<-droplevels(Reject)
    setkeyv(Reject, "interview__id")
    if(exists("gps_file_merge")) Reject<-gps_file_merge[Reject, on="interview__id"]
    Reject<-KeyAssigned_merge[Reject, on="interview__id"]
    para_data$Reject<-Reject
    ##  PASSIVE EVENTS (ONLY IF REQUESTED)
    if (!onlyActiveEvents) {
      ##  2.11. Invalid
      cat("\n\tQuestionDeclaredInvalid\n")
      QuestionDeclaredInvalid<-paradata_files[action=="QuestionDeclaredInvalid"]
      QuestionDeclaredInvalid<-QuestionDeclaredInvalid[!is.na(interview__id)]
      QuestionDeclaredInvalid[, count:=length(counter), by=interview__id]
      setkeyv(QuestionDeclaredInvalid, "interview__id")
      if(exists("gps_file_merge")) QuestionDeclaredInvalid<-gps_file_merge[QuestionDeclaredInvalid, on="interview__id"]
      QuestionDeclaredInvalid<-KeyAssigned_merge[QuestionDeclaredInvalid, on="interview__id"]
      para_data$QuestionDeclaredInvalid<-QuestionDeclaredInvalid
      ##  2.12. Valid
      cat("\n\tQuestionDeclaredValid\n")
      QuestionDeclaredValid<-paradata_files[action=="QuestionDeclaredValid"]
      QuestionDeclaredValid<-QuestionDeclaredValid[!is.na(interview__id),]
      QuestionDeclaredValid[, count:=length(counter), by=interview__id]
      para_data$QuestionDeclaredValid<-QuestionDeclaredValid
    }
    para_data[["actionDistr"]]<-actionDistr
    para_data[["userDistr"]]<-userDistr
    para_data[["roleDistr"]]<-roleDistr
    cat("\nExport & Transformation finished.\n")
    return(para_data)
  } else {
    ###############################
    ## MULTICORE: SET to 20gb for future parallel
    simu<-length(levels(droplevels(paradata_files$action)))

    options(future.globals.maxSize=15000*1024^2)
    multiCore <- min(simu, multiCore)
    cat("\nStarting Multicore with:\t", multiCore, " cores.\n")
    checkNum(multiCore)
    pack_dp_sp<-c("data.table")
    registerDoFuture()
    plan(sequential)

    para_dataMC<-foreach(i=1:simu, .packages = pack_dp_sp,
                         .combine=c,
                         .multicombine = T,
                         .export = c("para_data"),
                         #.verbose = T,
                         .errorhandling="pass") %dopar% {

                           ## progress
                           #p(sprintf("event = %g", event))
                           event<-levels(paradata_files$action)[i]
                           p(sprintf("i=%g", i))
                           if (event=="AnswerSet"){
                             AnswerSet<-para1_answer
                             AnswerSet<-AnswerSet[!is.na(interview__id)]
                             setkeyv(AnswerSet, "interview__id")
                             if(exists("gps_file_merge"))  AnswerSet<-gps_file_merge[AnswerSet, on="interview__id"]
                             AnswerSet<-KeyAssigned_merge[AnswerSet, on="interview__id"]
                             para_data[[event]]<-AnswerSet
                             rm(AnswerSet)

                           } else if (event=="AnswerRemoved"){
                             ##  2.9. Answer Removed (COUNT the number of Removed answer by questionnaire)
                             AnswerRemoved<-paradata_files[action=="AnswerRemoved"]
                             AnswerRemoved<-AnswerRemoved[!is.na(interview__id)]
                             AnswerRemoved[, count:=length(counter), by=interview__id]
                             AnswerRemoved[,c("responsible", "role"):=NULL]
                             AnswerRemoved<-droplevels(AnswerRemoved)
                             AnswerRemoved<-merge(AnswerRemoved, para1_answer_merge, by="interview__id", allow.cartesian=T)
                             setkeyv(AnswerRemoved, "interview__id")
                             if(exists("gps_file_merge")) AnswerRemoved<-gps_file_merge[AnswerRemoved, on="interview__id"]
                             AnswerRemoved<-KeyAssigned_merge[AnswerRemoved, on="interview__id"]
                             para_data[[event]]<-AnswerRemoved
                             rm(AnswerRemoved)
                           } else if (event=="ApproveByHeadquarter") {
                             ##  2.10. Approved
                             ApproveByHeadquarter<-paradata_files[action=="ApproveByHeadquarter"]
                             ApproveByHeadquarter<-droplevels(ApproveByHeadquarter)
                             ApproveBySupervisor<-paradata_files[action=="ApproveBySupervisor"]
                             ApproveBySupervisor<-droplevels(ApproveBySupervisor)
                             para_data[[event]]<-ApproveBySupervisor
                             rm(ApproveBySupervisor)
                           } else if (event=="QuestionDeclaredInvalid" & !onlyActiveEvents) {
                             ##  2.11. Invalid
                             QuestionDeclaredInvalid<-paradata_files[action=="QuestionDeclaredInvalid"]
                             QuestionDeclaredInvalid<-QuestionDeclaredInvalid[!is.na(interview__id)]
                             QuestionDeclaredInvalid[, count:=length(counter), by=interview__id]
                             setkeyv(QuestionDeclaredInvalid, "interview__id")
                             if(exists("gps_file_merge")) QuestionDeclaredInvalid<-gps_file_merge[QuestionDeclaredInvalid, on="interview__id"]
                             QuestionDeclaredInvalid<-KeyAssigned_merge[QuestionDeclaredInvalid, on="interview__id"]
                             para_data[[event]]<-QuestionDeclaredInvalid
                             rm(QuestionDeclaredInvalid)
                           } else if (event=="QuestionDeclaredValid" & !onlyActiveEvents) {
                             ##  2.12. Valid
                             QuestionDeclaredValid<-paradata_files[action=="QuestionDeclaredValid"]
                             QuestionDeclaredValid<-QuestionDeclaredValid[!is.na(interview__id),]
                             QuestionDeclaredValid[, count:=length(counter), by=interview__id]
                             para_data[[event]]<-QuestionDeclaredValid
                             rm(QuestionDeclaredValid)
                           } else if (event=="Restarted") {
                             ##  2.13 Restarted
                             Restarted<-paradata_files[action=="Restarted"]
                             Restarted<-Restarted[!is.na(interview__id),]
                             Restarted[, count:=length(counter), by=interview__id]
                             setkeyv(Restarted, "interview__id")
                             if(exists("gps_file_merge")) Restarted<-gps_file_merge[Restarted, on="interview__id"]
                             Restarted<-KeyAssigned_merge[Restarted, on="interview__id"]
                             para_data[[event]]<-Restarted
                             rm(Restarted)
                           } else if (event=="Reject") {
                             ##  2.14. Rejected
                             Reject<-paradata_files[action=="RejectedBySupervisor"|action=="RejectedByHeadquarter"][,c("var_resp", "rid"):=NULL]
                             setnames(Reject, "var", "comment")
                             Reject<-droplevels(Reject)
                             #paradata_files<-paradata_files[action!="RejectedBySupervisor"&action!="RejectedByHeadquarter"]
                             #paradata_files<-droplevels(paradata_files)
                             setkeyv(Reject, "interview__id")
                             if(exists("gps_file_merge")) Reject<-gps_file_merge[Reject, on="interview__id"]
                             Reject<-KeyAssigned_merge[Reject, on="interview__id"]
                             para_data[[event]]<-Reject
                             rm(Reject)
                           }
                           return(para_data)
                         }

    para_dataMC[["actionDistr"]]<-actionDistr
    para_dataMC[["userDistr"]]<-userDistr
    para_dataMC[["roleDistr"]]<-roleDistr
    return(para_dataMC)
  }
  ##########################################################################################################
}

#19.31   15.02  208.89


##############################################################################################
##    unpack
# unpack<-function(file.name=fp, gps1=gps[[1]][1], gps2=gps[[1]][2], gps3=gps[[1]][1]){
#   ##  1. Use fread from data.table (fastest, needs to be tested for reliability)
#   library(bigreadr)
#   file<-fread(file=file.name,
#               header = F,
#               skip = 1L,
#               fill = T,sep = "\t",
#               colClasses = list(factor=c(1L,3L:5L,7L),
#                                 numeric=c(2L),
#                                 character=c(6L,8L)),
#               nThread = 8,
#               blank.lines.skip=TRUE,
#               encoding = "UTF-8")
#
#   if (length(file)==9) {
#     ## OLD format
#     names(file)<-c("interview__id", "counter", "action", "responsible", "role", "time","var", "var_resp", "rid")
#     #CHECKfile<-file
#     resps<-file[,tstrsplit(var_resp, "(\\|)|(,)", fixed=F, names = T, fill = "<NA>")][]
#     splits <- (length(resps))
#     #file[,response1:=var_resp]
#     file[,c(paste0("response", 1:(length(resps)))):=resps]
#     rm(resps)
#   } else if (length(file)==8) {
#     names(file)<-c("interview__id", "counter", "action", "responsible", "role", "time", "tz","var_resp")
#     resps<-file[,tstrsplit(var_resp, "||", fixed=T, names = T, fill = "<NA>")][]
#     resps1<-resps[,tstrsplit(V2, "(\\|)|(,)", fixed=F, names = T, fill = "<NA>")][]
#     resps2<-resps[,tstrsplit(V3, "(,)", fixed=F, names = T, fill = "<NA>")][]
#     splits <- (length(resps1))
#     file[,c("var"):=resps[,.(V1)]]
#     file[,c(paste0("response", 1:(length(resps1)))):=resps1]
#     file[,c(paste0("rid", 1:(length(resps2)))):=resps2]
#   }
#   for (i in 1:splits) {
#     i<-paste0("response", i)
#     file[,c(i):=as.factor(get(i))]
#   }
#
#   file[,c("date", "time"):=tstrsplit(time, "T", fixed=TRUE)][]
#   file[,time:=as.ITime(time)]
#   file[,date:=as.IDate(date)]
#   file[,wDAY:=wday(date)]
#   file[,mDAY:=mday(date)]
#   file[,MONTH:=month(date)]
#   file[,WEEK:=isoweek(date)]
#   setkeyv(file, c("interview__id", "responsible"))
#   return(file)
#   prog<-prog+1
# }
