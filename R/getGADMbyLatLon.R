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
  # invisible(capture.output(
  # country<-revgeo.io(longitude = dt$long, latitude = dt$lat, output = "hash",item = "country")
  # ))
  dt<-dt[!is.na(lat) & !is.na(long)]
  country<-MazamaSpatialUtils::getCountry(longitude = dt$long, latitude = dt$lat, allData = T)
  country<-MazamaSpatialUtils::iso2ToIso3(country$countryCode)
  country<-stat_mode(country)
  ## exceptions
  if(country== "Country Not Found") stop("No Country Available!")
  if (country=="United States of America") country<-"United States"

  ######################
  ##  get ISO3 cc
  ##  modify errors manually
  #country<-data.table(getData("ISO3"))[NAME==country,"ISO3"]

  shp.out<-GADM.getData(country = country, sp.Library = sp.Library, level = level, path = path)

  #########################
  ##  Convert to sf option

  if (!is.null(aggregation.var)) {
    dt<-copy(GpsData)
    dt<-dt[!is.na(long)&!is.na(lat)]
    dt<-dt[!is.nan(long)&!is.nan(lat)]
    #sp::coordinates(dt)<-~long+lat
    dt<-sf::st_as_sf(dt, coords = c("long", "lat"), crs = 4326)
    shp.out<-st_transform(shp.out, 3857)
    dt<-sf::st_transform(dt, 3857)
    dt<-dt[,aggregation.var]
    dt<-st_join(dt, shp.out, join=st_within)
    dt<-as.data.frame(dt[,c("NAME_2", aggregation.var)])
    dt<-data.table(dt)
    dt<-dt[,.(Aggregate=round(mean(get(aggregation.var), na.rm=T), 2), Count = sum(!is.na(get(aggregation.var)))), by=.(NAME_2)]
    shp.out<-merge(shp.out, dt, by=c("NAME_2"))
  }
  return(shp.out)
}

# statistical mode --> in server util
# stat_mode<-function(x) {
#   # mode solution from: https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
#   ux<-unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
