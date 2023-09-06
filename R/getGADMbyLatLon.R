############################
##  load gadm map based on coordinates
##############################


getGADMbyCoord<-function(GpsData=NULL, ss=20, aggregation.var="durationNOBREAK",
                         sp.Library = "sf", level = 2, path = "./data/pilot/") {

  ##################################
  ##  1. Coordinate Data is copied
  ##  2. Coordinate names must be named lat/long
  ##  3. if sf option is chosen, sf must be installed
  ##  4. ss is how many points to sample for country identification.
  ##  5. if variable is specified, then aggregated within shapes
  ##################################
  dt<-copy(GpsData)
  ss<-min(nrow(dt), ss)
  dt<-dt[sample.int(.N, ss)]
  invisible(capture.output(
  country<-revgeo.io(longitude = dt$long, latitude = dt$lat, output = "hash",item = "country")
  ))
  country<-modal(country$country)
  ## exceptions
  if(country== "Country Not Found") stop("No Country Available!")
  if (country=="United States of America") country<-"United States"

  ######################
  ##  get ISO3 cc
  ##  modify errors manually
  country<-data.table(getData("ISO3"))[NAME==country,"ISO3"]

  shp.out<-GADM.getData(country = country[1,1], sp.Library = sp.Library, level = level, path = path)

  #########################
  ##  Convert to sf option

  if (!is.null(aggregation.var)) {
    dt<-copy(GpsData)
    dt<-dt[!is.na(long)&!is.na(lat)]
    dt<-dt[!is.nan(long)&!is.nan(lat)]
    sp::coordinates(dt)<-~long+lat
    dt<-st_as_sf(dt)
    dt<-st_set_crs(dt, st_crs(shp.out))
    shp.out<-st_transform(shp.out, 3857)
    dt<-st_transform(dt, 3857)
    dt<-dt[,aggregation.var]
    dt<-st_join(dt, shp.out, join=st_within)
    dt<-as.data.frame(dt[,c("OBJECTID", "NAME_2", aggregation.var)])
    dt<-data.table(dt)
    dt<-dt[,.(Aggregate=round(mean(get(aggregation.var), na.rm=T), 2), Count = sum(!is.na(get(aggregation.var)))), by=.(OBJECTID, NAME_2)]
    shp.out<-merge(shp.out, dt, by=c("OBJECTID", "NAME_2"))
  }
  return(shp.out)
}

#############################################
##  New function to load GADM data
##  replaces getData in raster for GADM only

# https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_AUT_0_sp.rds
# https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_AUT_1_sp.rds

GADM.getData<-function(name="GADM",country,level,path, sp.Library="sp"){
  version<-"3.6"
  v<-as.numeric(version)*10
  DIR<-ifelse(sp.Library=="sp", "/Rsp/", "/Rsf/")
  tf<-tempfile()

  ##  1. Create path
  file.name<-paste0("gadm", v, "_", country, "_", level, "_", sp.Library, ".rds")
  f.path<-file.path(path, file.name)
  url <- paste0("https://biogeo.ucdavis.edu/data/gadm", version, DIR, file.name)
  print(url)

  ##  2. check local availability
  if (file.exists(f.path)) {
    GADM.map<- readRDS(f.path)
    return(GADM.map)
  } else {
    GADM.header<-GET(url, write_disk(f.path))
    print(GADM.header)
    if (GADM.header$status_code==200){
      GADM.map<- readRDS(f.path)
      return(GADM.map)
    } else {
      "No Map available! Are you sure you your request is correctly specified?"
    }
  }
}

