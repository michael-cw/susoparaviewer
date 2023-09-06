#' Reverse geo code helper
#'
#' @noRd
#' @keywords internal
#'

revgeo.io <- function(longitude, latitude, provider = NULL, API = NULL,
                      output = NULL, item = NULL) {
  if (missing(provider)) {
    provider <- NULL
  }
  if (missing(API)) {
    API <- NULL
  }
  if (missing(output)) {
    output <- NULL
  }
  if (missing(item)) {
    item <- NULL
  }
  geocode_data <- list()
  geocode_frame <- data.frame()
  if (is.null(provider) || (provider %in% "photon")) {
    url <- paste0(
      "https://photon.komoot.io/reverse?lon=",
      longitude, "&lat=", latitude
    )

    for (i in (url)) {
      print(paste0(
        "Getting geocode data from Photon: ",
        i
      ))
      data <- httr::GET(url = i)
      returned_data <- tryCatch(fromJSON(httr::content(data)), error = function(e) "There was an issue retrieving an address from Photon.  Please check that your coordinates are coorect and try again.")
      housenumber <- tryCatch(returned_data$features[[1]]$properties$housenumber,
        error = function(e) "House Number Not Found"
      )
      street <- tryCatch(returned_data$features[[1]]$properties$street,
        error = function(e) "Street Not Found"
      )
      city <- tryCatch(returned_data$features[[1]]$properties$city,
        error = function(e) "City Not Found"
      )
      zip <- tryCatch(returned_data$features[[1]]$properties$postcode,
        error = function(e) "Postcode Not Found"
      )
      state <- tryCatch(returned_data$features[[1]]$properties$state,
        error = function(e) "State Not Found"
      )
      country <- tryCatch(returned_data$features[[1]]$properties$country,
        error = function(e) "Country Not Found"
      )
      if (is.null(housenumber)) {
        housenumber <- "House Number Not Found"
      }
      if (is.null(street)) {
        street <- "Street Not Found"
      }
      if (is.null(city)) {
        city <- "City Not Found"
      }
      if (is.null(zip)) {
        zip <- "Postcode Not Found"
      }
      if (is.null(state)) {
        state <- "State Not Found"
      }
      if (is.null(country)) {
        country <- "Country Not Found"
      }
      if (is.null(output)) {
        geocode_data <- append(geocode_data, paste(paste0(
          housenumber,
          " ", street
        ), city, state, zip, country, sep = ", "))
      } else if (output == "string") {
        geocode_data <- append(geocode_data, paste(paste0(
          housenumber,
          " ", street
        ), city, state, zip, country, sep = ", "))
      } else if (output == "hash") {
        geocode_data[["housenumber"]] <- c(
          geocode_data[["housenumber"]],
          housenumber
        )
        geocode_data[["street"]] <- c(
          geocode_data[["street"]],
          street
        )
        geocode_data[["city"]] <- c(
          geocode_data[["city"]],
          city
        )
        geocode_data[["state"]] <- c(
          geocode_data[["state"]],
          state
        )
        geocode_data[["zip"]] <- c(
          geocode_data[["zip"]],
          zip
        )
        geocode_data[["country"]] <- c(
          geocode_data[["country"]],
          country
        )
      } else {
        geocode_data[["housenumber"]] <- c(
          geocode_data[["housenumber"]],
          housenumber
        )
        geocode_data[["street"]] <- c(
          geocode_data[["street"]],
          street
        )
        geocode_data[["city"]] <- c(
          geocode_data[["city"]],
          city
        )
        geocode_data[["state"]] <- c(
          geocode_data[["state"]],
          state
        )
        geocode_data[["zip"]] <- c(
          geocode_data[["zip"]],
          zip
        )
        geocode_data[["country"]] <- c(
          geocode_data[["country"]],
          country
        )
        geocode_frame <- rbind(geocode_frame, as.data.frame(geocode_data))
      }
    }
  } else if (is.null(API) && provider %in% "Bing") {
    print("Please enter your Bing api")
    return()
  } else if (provider %in% "bing") {
    url <- paste0(
      "http://dev.virtualearth.net/REST/v1/Locations/",
      latitude, ",", longitude, "?o=json&key=", API
    )
    if (!(is.null(item))) {
      if (item %in% "housenumber") {
        print("Defaulting to street, since Bing returns house numbers and streets together.")
        item <- "street"
      }
    }
    for (i in url) {
      print(paste0(
        "Getting geocode data from Bing: ",
        i
      ))
      data <- RCurl::getURLAsynchronous(i)
      returned_data <- tryCatch(fromJSON(data), error = function(e) "There was an issue retrieving an address from Bing.  Please check that your coordinates are coorect and try again.")
      address_hash <- tryCatch(as.list(returned_data$resourceSets[[1]]$resources[[1]]$address),
        error = function(e) "House Number Not Found"
      )
      street <- tryCatch(address_hash$addressLine, error = function(e) "House Number and Street Not Found")
      city <- tryCatch(address_hash$locality, error = function(e) "City Not Found")
      state <- tryCatch(address_hash$adminDistrict, error = function(e) "State Not Found")
      zip <- tryCatch(address_hash$postalCode, error = function(e) "Postcode Not Found")
      country <- tryCatch(address_hash$countryRegion,
        error = function(e) "Country Not Found"
      )
      address_string <- tryCatch(address_hash$formattedAddress,
        error = function(e) "Address Number Not Found"
      )
      if (is.null(street)) {
        street <- "House Number and Street Not Found"
      }
      if (is.null(city)) {
        city <- "City Not Found"
      }
      if (is.null(zip)) {
        zip <- "Postcode Not Found"
      }
      if (is.null(state)) {
        state <- "State Not Found"
      }
      if (is.null(country)) {
        country <- "Country Not Found"
      }
      if (is.null(output)) {
        geocode_data <- append(geocode_data, address_string)
      } else if (output == "string") {
        geocode_data <- append(geocode_data, address_string)
      } else if (output == "hash") {
        geocode_data[["street"]] <- c(
          geocode_data[["street"]],
          street
        )
        geocode_data[["city"]] <- c(
          geocode_data[["city"]],
          city
        )
        geocode_data[["state"]] <- c(
          geocode_data[["state"]],
          state
        )
        geocode_data[["zip"]] <- c(
          geocode_data[["zip"]],
          zip
        )
        geocode_data[["country"]] <- c(
          geocode_data[["country"]],
          country
        )
      } else {
        geocode_data[["street"]] <- c(
          geocode_data[["street"]],
          street
        )
        geocode_data[["city"]] <- c(
          geocode_data[["city"]],
          city
        )
        geocode_data[["state"]] <- c(
          geocode_data[["state"]],
          state
        )
        geocode_data[["zip"]] <- c(
          geocode_data[["zip"]],
          zip
        )
        geocode_data[["country"]] <- c(
          geocode_data[["country"]],
          country
        )
        geocode_frame <- rbind(geocode_frame, as.data.frame(geocode_data))
      }
    }
  } else if (is.null(API) && provider %in% "google") {
    print("Please enter your Google api")
    return()
  } else if (provider %in% "google") {
    url <- paste0(
      "https://maps.googleapis.com/maps/api/geocode/json?latlng=",
      latitude, ",", longitude, "&key=", API
    )
    postcode <- list()
    for (i in url) {
      print(paste0(
        "Getting geocode data from Google: ",
        i
      ))
      data <- RCurl::getURLAsynchronous(i)
      returned_data <- tryCatch(fromJSON(data), error = function(e) "There was an issue retrieving an address from Google Maps.  Please check that your coordinates are coorect and try again.")
      if (returned_data$status %in% "REQUEST_DENIED") {
        print("There was an error accessing Google Maps.  Check your API key and try again.")
        return()
      }
      address <- returned_data$results[[1]]$formatted_address
      l <- length(returned_data$results[[1]]$address_components)
      k <- 1
      while (k <= l) {
        j <- returned_data$results[[1]]$address_components[[k]]
        if (j$types[1] == "street_number") {
          housenumber <- tryCatch(j$short_name, error = function(e) "House Number Not Found")
        } else if (j$types[1] == "route") {
          street <- tryCatch(j$long_name, error = function(e) "Street Not Found")
        } else if (j$types[1] == "locality") {
          city <- tryCatch(j$long_name, error = function(e) "City Not Found")
        } else if (j$types[1] == "administrative_area_level_2") {
          county <- tryCatch(j$long_name, error = function(e) "County Not Found")
        } else if (j$types[1] == "postal_code") {
          zip <- tryCatch(j$long_name, error = function(e) "Postcode Not Found")
        } else if (j$types[1] == "administrative_area_level_1") {
          state <- tryCatch(j$long_name, error = function(e) "State Not Found")
        } else if (j$types[1] == "country") {
          country <- tryCatch(j$long_name, error = function(e) "State Not Found")
        }
        k <- k + 1
      }
      if (!(exists("housenumber"))) {
        housenumber <- "House Number Not Found"
      }
      if (!(exists("street"))) {
        street <- "Street Not Found"
      }
      if (!(exists("city"))) {
        city <- "City Not Found"
      }
      if (!(exists("zip"))) {
        zip <- "Postcode Not Found"
      }
      if (!(exists("state"))) {
        state <- "State Not Found"
      }
      if (!(exists("country"))) {
        country <- "Country Not Found"
      }
      if (is.null(output)) {
        geocode_data <- append(geocode_data, address)
      } else if (output == "string") {
        geocode_data <- append(geocode_data, address)
      } else if (output == "hash") {
        geocode_data[["housenumber"]] <- c(
          geocode_data[["housenumber"]],
          housenumber
        )
        geocode_data[["street"]] <- c(
          geocode_data[["street"]],
          street
        )
        geocode_data[["city"]] <- c(
          geocode_data[["city"]],
          city
        )
        geocode_data[["county"]] <- c(
          geocode_data[["country"]],
          country
        )
        geocode_data[["state"]] <- c(
          geocode_data[["state"]],
          state
        )
        geocode_data[["zip"]] <- c(
          geocode_data[["zip"]],
          zip
        )
        geocode_data[["country"]] <- c(
          geocode_data[["country"]],
          country
        )
      } else {
        geocode_data[["housenumber"]] <- c(
          geocode_data[["housenumber"]],
          housenumber
        )
        geocode_data[["street"]] <- c(
          geocode_data[["street"]],
          street
        )
        geocode_data[["city"]] <- c(
          geocode_data[["city"]],
          city
        )
        geocode_data[["county"]] <- c(
          geocode_data[["country"]],
          country
        )
        geocode_data[["state"]] <- c(
          geocode_data[["state"]],
          state
        )
        geocode_data[["zip"]] <- c(
          geocode_data[["zip"]],
          zip
        )
        geocode_data[["country"]] <- c(
          geocode_data[["country"]],
          country
        )
        geocode_frame <- rbind(geocode_frame, as.data.frame(geocode_data))
      }
    }
  } else {
    print("Please enter a provider (photon, google, or bing)")
  }
  if (!nrow(geocode_frame)) {
    if (!(is.null(output %in% "hash"))) {
      if (is.null(item)) {
        return(geocode_data)
      } else {
        return(geocode_data[item])
      }
    } else {
      return(geocode_data)
    }
  } else {
    return(as.data.frame(geocode_data))
  }
}
