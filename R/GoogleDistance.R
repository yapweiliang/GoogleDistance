# GoogleDistances.R


# private constants -------------------------------------------------------

.simplified_uk_postcode_regex <- "(?:[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2})"

# initialise --------------------------------------------------------------

cacheEnv <- new.env()
assign("API_key", "", envir = cacheEnv) # no key specified by default

# set / get GoogleAPIKey --------------------------------------------------

#' @describeIn getGoogleDistance Assign and remember your API Key
#' @param key \code{string} containing the API key
#' @export
#' @examples \dontrun{setGoogleAPIKey(key = readLines("H:/path/to/MyGoogleAPIKey.txt", warn = FALSE))}
setGoogleAPIKey <- function(key) {
  assign("API_key", key, envir = cacheEnv)
}

#' @describeIn getGoogleDistance Get my Google API Key from cache
#' @return \code{getGoogleAPIKey} returns the stored key (string)
#' @export
#' @examples getGoogleAPIKey()
getGoogleAPIKey <- function() {
  return(get("API_key", envir = cacheEnv))
}

# getGoogleDistance -------------------------------------------------------

#' Get Google Distances
#'
#' Get distance, duration, and traffic duration from the Google Maps Distance Matrix API
#'
#' \url{https://developers.google.com/maps/documentation/distance-matrix/}
#'
#' An API key is required for traffic durations.
#'
#' Date string is converted to POSIXct using as.POSIXct using the default time zone;
#' see \code{\link[base]{as.POSIXct}} for details.
#'
#' @param origin,destination string containing postcode or lat long coordinates.  also accepts address
#' @param is.latlong if not coordinates, then ",UK" is appended to the origin and destination string
#' @param imperial \code{TRUE} to return miles instead of kilometres
#' @param verbose show the URL sent to Google?
#' @param very_verbose show the XML file returned?
#' @param show_warnings show mild warnings?
#' @param delay between queries (seconds).  The Standard Usage Limits allow 100 elements per 10 seconds (= 0.1s)
#' @param user_confirm If postcode provided does not match postcode returned, prompt user to accept, or automatically bail out?
#' @param traffic_model Google accepts \code{"best_guess"}, \code{"pessimistic"}, or, \code{"optimistic"}
#' @param departure_time Leave as \code{NULL} to choose next Monday 8am (generates warning), or specify date string e.g. "2016-12-25 22:00"
#' @param not_found_result Return value if results not found, e.g. \code{-1}, or \code{NA}
#'
#' @return \code{getGoogleDistance} returns a list containing distance, duration and traffic_duration (numeric), or \code{not_found_result} value if not found.
#' @export
#'
#' @examples
#' getGoogleDistance("DT1 2JY", "BA21 4AT")
#' setGoogleAPIKey("silly-key")
#' \dontrun{getGoogleDistance("BA21 4AT", "DT1 2JY") # generates error with the silly-key}
#'
getGoogleDistance <- function(origin, destination,
                              is.latlong = FALSE,
                              imperial = TRUE,
                              verbose = FALSE,
                              very_verbose = FALSE,
                              show_warnings = FALSE,
                              delay = 0.1,
                              user_confirm = TRUE,
                              traffic_model = "pessimistic",
                              departure_time = NULL,
                              not_found_result = -1) {

  # http://stackoverflow.com/questions/16863018/getting-driving-distance-between-two-points-lat-lon-using-r-and-google-map-ap
  # https://developers.google.com/maps/documentation/distancematrix/

  # expect UK postcode, or latlong
  # also accept UK address but this will likely trigger manual checking

  if (is.latlong) add_UK <- "" else add_UK <- ",UK"

  keystring <- get("API_key", cacheEnv)
  if (keystring == "") warning("No API key specified - will not be able to obtain traffic durations.")

  if (traffic_model %in% c("best_guess", "pessimistic", "optimistic")) {
    trafficstring <- paste0("&traffic_model=", traffic_model)
  } else {
    stop(paste0("'", traffic_model, "' is an unrecognised traffic_model."))
  }

  if (is.null(departure_time)) {
    # if no date/time specified, choose next Monday 8am
    d <- as.POSIXct(cut.Date(Sys.Date() + 7,"weeks")) + lubridate::hours(8) # get next Monday 8am
    if (show_warnings) warning( paste0("No departure_time specified, therefore using next Monday 8am (", d, ")"))
    d <- as.numeric(d)
  } else {
    d <- as.numeric(as.POSIXct(departure_time))
  }
  departurestring <- paste0("&departure_time=", d)

  xml.url <-
    URLencode(paste0(
      'https://maps.googleapis.com/maps/api/distancematrix/xml?',
      '&origins=', origin, add_UK,
      '&destinations=', destination, add_UK,
      '&mode=driving',
      departurestring,
      trafficstring,
      '&key=', keystring
    ))
  xmlfile <- XML::xmlParse(RCurl::getURL(xml.url)) # TODO trap error here if URL is so malformed that Google does not accept

  bailout <- FALSE

  # check if Google is happy
  status1 <- XML::xmlValue(XML::xpathApply(xmlfile,"//status")[[1]])
  if (status1 != 'OK') {
    # Top-level Status Codes OK, INVALID_REQUEST, MAX_ELEMENTS_EXCEEDED, OVER_QUERY_LIMIT, REQUEST_DENIED, UNKNOWN_ERROR
    error_message <- XML::xmlValue(XML::xpathApply(xmlfile,"//error_message")[[1]])
    if (very_verbose) error_message <- paste0(error_message, '\n', XML::toString.XMLNode(xmlfile))
    stop('SENT: ',xml.url,'\n  RECEIVED(1): ',status1,', ',error_message)
  }
  status2 <- XML::xmlValue(XML::xpathApply(xmlfile,"//status")[[2]])
  if (status2 != 'OK') {
    # Element-level Status Codes OK, NOT_FOUND, ZERO_RESULTS
    if (very_verbose) xmldump <- paste0("\n", XML::toString.XMLNode(xmlfile)) else xmldump <- ""
    stop('SENT: ', xml.url, '\n  RECEIVED(2): ', status2, xmldump)
  }

  # feedback
  if (very_verbose || verbose) message(xml.url)
  if (very_verbose) print(xmlfile)

  # checking
  if (is.latlong) {
    # not sure if we can do any error checking here...
  } else
  {
    # check that it has understood our request
    test_destination <- XML::xmlValue(XML::xpathApply(xmlfile,"//destination_address")[[1]])
    test_origin <-      XML::xmlValue(XML::xpathApply(xmlfile,"//origin_address")[[1]])
    # extract compressed postcode
    test_destination_pc <- gsub(" ", "", stringr::str_match(test_destination, .simplified_uk_postcode_regex), fixed = TRUE)
    test_origin_pc      <- gsub(" ", "", stringr::str_match(test_origin,      .simplified_uk_postcode_regex), fixed = TRUE)
    # extract compressed postcode
    destination_pc <- gsub(" ", "", stringr::str_match(destination, .simplified_uk_postcode_regex), fixed = TRUE)
    origin_pc      <- gsub(" ", "", stringr::str_match(origin,      .simplified_uk_postcode_regex), fixed = TRUE)

    if ( (is.na(test_origin_pc)) || (is.na(origin_pc)) || (test_origin_pc != origin_pc) ) {
      error_str <- paste0("origin '",origin,"' not understood by Google API, returned '",test_origin,"'.")
      if (user_confirm) {
        message(error_str)
        ANSWER <- readline(paste0("(a)ccept Google's suggestion or (b)ailout and return '",not_found_result,"'?"))
        if (substr(ANSWER,1,1) == 'a') {
          bailout <- FALSE
          warning("Manual confirmation accepted as:", error_str)
        } else {
          bailout <- TRUE
        }
      } else {
        bailout <- TRUE
      }
    }
    if ( !bailout && ((is.na(test_destination_pc)) || (is.na(destination_pc)) || (test_destination_pc != destination_pc)) ) {
      error_str <- paste0("destination '",destination,"' not understood by Google API, returned '",test_destination,"'.")
      if (user_confirm) {
        message(error_str)
        ANSWER <- readline(paste0("(a)ccept Google's suggestion or (b)ailout and return '",not_found_result,"'?"))
        if (substr(ANSWER,1,1) == 'a') {
          bailout <- FALSE
          warning("Manual confirmation accepted as:", error_str)
        } else {
          bailout <- TRUE
        }
      } else {
        bailout <- TRUE
      }
    }
  } # end if (is.latlong)

  if (bailout) {
    warning("Bailing out and returning '",not_found_result,"' because: ",error_str)
    mi <- du <- dt <- not_found_result
  } else {
    # delay
    Sys.sleep(delay)

    # get distance and duration
    if (imperial) divisor <- 1600 else divisor <- 1000
    mi <- as.numeric(XML::xmlValue(XML::xpathApply(xmlfile, "//distance//value")[[1]])) / divisor # metres to miles or km
    du <- as.numeric(XML::xmlValue(XML::xpathApply(xmlfile, "//duration//value")[[1]])) / 60 # seconds to minutes
    dt <- as.numeric(XML::xmlValue(unlist(XML::xpathApply(xmlfile, "//duration_in_traffic/value"))[[1]])) / 60 # a little more code to handle missing value (if no API key)
    if (is.na(dt)) dt <- not_found_result
  }

  return(as.list(c(distance = mi, duration = du, traffic_duration = dt)))
}

