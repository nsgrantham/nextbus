
nextbus_json_feed_url <- "http://webservices.nextbus.com/service/publicJSONFeed?command="

#' @importFrom jsonlite fromJSON
read_json <- function(url) {
  fromJSON(readLines(url))
}

get_agency_list <- function() {
  read_json(paste0(nextbus_json_feed_url, "agencyList"))
}

get_route_list <- function(agency_tag) {
  read_json(paste0(nextbus_json_feed_url, "routeList&a=", agency_tag))
}

get_route_config <- function(agency_tag, route_tag = NULL) {
  url <- paste0(nextbus_json_feed_url, "routeConfig&a=", agency_tag)
  if (!is.null(route_tag)) {
    url <- paste0(url, "&r=", route_tag)
  }
  read_json(url)
}

get_predictions <- function(agency_tag, route_tag, stop_tag) {
  url <- paste0(nextbus_json_feed_url, "predictions&a=", agency_tag,
                "&r=", route_tag, "&s=", stop_tag)
  read_json(url)
}

get_predictions_for_multi_stops <- function(agency_tag, route_tags, stop_tags) {
  if (length(route_tags) == 1) {
    route_tags <- rep(route_tags, length(stop_tags))
  }
  stopifnot(length(route_tags) != length(stop_tags))
  url <- paste0(nextbus_json_feed_url, "predictions&a=", agency_tag,"&r=", route_tag)
  for (i in seq_along(stop_tags)) {
    url <- paste0(url, "&stops=", route_tags[i], "|", stop_tags[i])
  }
  read_json(url)
}

get_schedule <- function(agency_tag, route_tag) {
  read_json(paste0(nextbus_json_feed_url, "schedule&a=", agency_tag, "&r=", route_tag))
}

get_messages <- function(agency_tag, route_tags) {
  url <- paste0(nextbus_json_feed_url, "messages&a=", agency_tag)
  for (route_tag in route_tags) {
    url <- paste0(url, "&r=", route_tag)
  }
  read_json(url)
}

get_vehicle_location <- function(agency_tag, vehicle_id) {
  read_json(paste0(nextbus_json_feed_url, "schedule&a=", agency_tag, "&v=", vehicle_id))
}

get_vehicle_locations <- function(agency_tag, route_tag, datetime = NULL) {
  if (is.null(datetime)) {
    epoch_time_in_msec = 0  # will provide data for the last 15 minutes
  } else {
    epoch_time_in_msec = ...
  }
  url <- paste0(nextbus_json_feed_url, "vehicleLocations&a=", agency_tag,
                "&r=", route_tag, "&t=", epoch_time_in_msec)
  data <- read_json(url)
  data$vehicle %>%
    as_tibble() %>%
    select(vehicle_id = id, route_tag = routeTag, lat, lon, predictable, speed_km_hr = speedKmHr,
           dir_tag = dirTag, heading, secs_since_report = secsSinceReport, leading_vehicle_id = leadingVehicleId)
}

