#' Function to retrieve postgres table
#'
#'
#' @keywords internal
#' @noRd

retrieve_postgres_table <- function(postgres_url,
                                    postgres_port,
                                    postgres_user,
                                    postgres_password,
                                    postgres_db_name,
                                    postgres_table_name,
                                    listTables = FALSE,
                                    asDataTable = TRUE) {

  # check connection
  canconect <- DBI::dbCanConnect(drv = RPostgres::Postgres(),
                                 dbname = postgres_db_name,
                                 host = postgres_url,
                                 port = 5432,
                                 user = postgres_user,
                                 password = postgres_password,
                                 sslmode = "require")

  stopifnot(canconect)

  # create a connection to the PostgreSQL database
  con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                        dbname = postgres_db_name,
                        host = postgres_url,
                        port = 5432,
                        user = postgres_user,
                        password = postgres_password,
                        sslmode = "require")

  if(listTables) {
    alltable<-DBI::dbListTables(conn = con)
    return(alltable)
  } else {
    # retrieve the specified table using dplyr
    pgtable <- dplyr::tbl(con, postgres_table_name) %>%
      dplyr::collect()

    if(asDataTable) pgtable<-data.table::as.data.table(pgtable)

    # disconnect from the database
    DBI::dbDisconnect(con)

    # return the table
    return(pgtable)
  }
}

#' Function to create SF line from points
#'
#'
#' @keywords internal
#' @noRd

pointsToTrackSF<-function(trackData) {
  # GPSLOGGER CONFIG
  #  - suso usernames are put into config file under id parameter
  #  - process:
  #             - retrieve users from server
  #             - select team or all users
  #             - name individual config file: sv_int.properties

  q1<-data.table::copy(trackData); rm(trackData)
  ####################
  ##  B. QUERY by user
  q1[,acc:=as.numeric(acc)]
  q1[,batt:=as.numeric(batt)]
  q1[,lat:=as.numeric(lat)]
  q1[,long:=as.numeric(long)]
  #####################################################
  ## create group and user from tid string when from gpsLogger

  q1[,time:=suppressWarnings(strptime(time, "%Y-%m-%d %H:%M:%S"))]
  q1[,c("DATE1", "TIME"):=data.table::IDateTime(time)]
  q1[,time:=NULL]
  crs_main<-4326
  # Drop less NA (lat/long)
  q1<-q1[!is.na(long)&!is.na(lat)]

  ##  Drop if less than 2 observations
  q1[,n:=.N, by=.(aid, id, DATE1)]
  q1<-q1[n>2]
  q1<-droplevels(q1)
  ##  Drop if DATE1/user/group is missing
  q1<-q1[!is.na(DATE1)];q1<-q1[!is.na(aid)];q1<-q1[!is.na(id)]
  # for (i in levels(q1$tid)){
  dftrL<-data.table::copy(q1); rm(q1)
  #dftrL[,timeDiff:=(max(TIME, na.rm = T)-min(TIME, na.rm = T))/60, by=.(tid, DATE)]
  data.table::setorderv(dftrL, c("aid", "DATE1", "TIME"))
  ####################################
  ##  spatial for later
  ##  a. points
  #print(nrow(dftrL))
  #print(table(dftrL$tid, dftrL$DATE1))
  if (nrow(dftrL)==0) return(NULL)
  dftrL_sf<-dftrL %>%
    sf::st_as_sf(coords = c("long","lat")) %>%
    sf::st_set_crs(crs_main)
  ## b. lines
  q1<-dftrL_sf %>%
    dplyr::group_by(id, aid, DATE1) %>%
    dplyr::filter(dplyr::n()>1) %>%
    dplyr::summarize(timeDiff = (max(TIME, na.rm = T)-min(TIME, na.rm = T))/60, n=dplyr::n()) %>%
    #st_cast("LINESTRING")
    sf::st_cast("MULTILINESTRING")
}


#' Function to calculate SF line length in KM
#'
#'
#' @keywords internal
#' @noRd

calculate_length_km <- function(sf_object) {
  # Check if the sf_object is of the correct geometry type
  if (unique(sf::st_geometry_type(sf_object)) != "MULTILINESTRING") {
    stop("The sf object must be of geometry type MULTILINESTRING.")
  }

  # If the sf object is not in a projected CRS, transform it to EPSG:3857 (meters)
  if (sf::st_is_longlat(sf_object)) {
    sf_object <- sf::st_transform(sf_object, 3857)
  }

  # Calculate the length in kilometers and add it as a new column
  sf_object <- sf_object %>%
    dplyr::mutate(distKM = sf::st_length(geometry) / 1000) %>%
    sf::st_transform(4326)

  return(sf_object)
}

#' Function to get center coords from line
#'
#'
#' @keywords internal
#' @noRd

calculate_center_point <- function(sf_object) {
  # Check if the sf_object is of the correct geometry type
  if (unique(sf::st_geometry_type(sf_object)) != "MULTILINESTRING") {
    stop("The sf object must contain geometry type MULTILINESTRING.")
  }

  # Ungroup if it is a grouped data frame
  sf_object <- dplyr::ungroup(sf_object)

  # If the sf object is not in long-lat CRS, transform it to EPSG:4326 (long-lat)
  if (!sf::st_is_longlat(sf_object)) {
    sf_object <- sf::st_transform(sf_object, 4326)
  }

  # Calculate the centroid for each MULTILINESTRING
  centers <- sf::st_centroid(sf::st_geometry(sf_object))

  # Extract latitude and longitude from the centroid
  coords <- sf::st_coordinates(centers)
  lat_lon <- data.table::data.table(lat = coords[, "Y"], long = coords[, "X"])

  # Convert sf object to data.table and bind with the latitude and longitude columns
  dt_object <- cbind(data.table::as.data.table(sf_object %>% sf::st_set_geometry(NULL)), lat_lon)

  return(dt_object)
}




#' Function to generate GPS logger config files
#'
#'
#' @keywords internal
#' @noRd
write_config <- function(gpsloggerserver = NULL,
                         gpsloggerserverport = 8080,
                         supervisor = NULL,
                         interviewer = NULL,
                         gpsloggerserver_username = NULL,
                         gpsloggerserver_pass = NULL,
                         loggingdistance = 10,
                         logginginterval = 60,
                         gpsprecision = 10,
                         fp = NULL,
                         fptemplate = NULL) {
  # function to escape url chars
  escape_url_characters <- function(url) {
    # List of characters to be escaped
    characters_to_escape <- c(":", "=")

    # Escape each character in the list
    for (char in characters_to_escape) {
      url <- gsub(char, paste0("\\", char), url, fixed = TRUE)
    }

    return(url)
  }
  # check filepath
  if(!dir.exists(fp)) dir.create(fp, showWarnings = F, recursive = T)
  # build user name
  interviewer<-paste(supervisor, interviewer, sep = "_")

  # build full url
  gpsloggerserver<-httr::modify_url(
    url = gpsloggerserver,
    scheme = "https",
    port = gpsloggerserverport,
    path = "log",
    query = list(
      lat=I("%LAT"),
      longitude=I("%LON"),
      time=I("%TIME"),
      s=I("%SPD"),
      id=interviewer,
      acc=I("%ACC"),
      aid=I("%AID"),
      batt=I("%BATT")
    )
  )
  gpsloggerserver<-escape_url_characters(gpsloggerserver)
  #gpsloggerserver<-httr::build_url(gpsloggerserver)
  # Read the template file
  lines <- readLines(fptemplate)  # Replace with the actual file path if different

  # Check for each line if it contains one of the keys to be updated
  updated_lines <- sapply(lines, function(line) {
    if (grepl("^log_customurl_url=", line)) {
      return(paste0("log_customurl_url=", gpsloggerserver))
    } else if (grepl("^current_profile_name=", line)) {
      return(paste0("current_profile_name=", interviewer))
    } else if (grepl("^log_customurl_basicauth_password=", line)) {
      return(paste0("log_customurl_basicauth_password=", gpsloggerserver_pass))
    } else if (grepl("^log_customurl_basicauth_username=", line)) {
      return(paste0("log_customurl_basicauth_username=", gpsloggerserver_username))
    } else if (grepl("^distance_before_logging=", line)) {
      return(paste0("distance_before_logging=", loggingdistance))
    } else if (grepl("^time_before_logging=", line)) {
      return(paste0("time_before_logging=", logginginterval))
    } else if (grepl("^accuracy_before_logging=", line)) {
      return(paste0("accuracy_before_logging=", gpsprecision))
    } else {
      return(line)  # If the line does not contain any key to be updated, return it as it is
    }
  }, USE.NAMES = FALSE)

  # Write the updated lines to a new file in the current working directory
  writeLines(updated_lines, file.path(fp, paste(interviewer,"properties", sep = ".")))
}

