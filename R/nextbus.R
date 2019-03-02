
nextbus_json_feed_url <- "http://webservices.nextbus.com/service/publicJSONFeed?command="

#' @importFrom jsonlite fromJSON
read_json <- function(url) {
  fromJSON(readLines(url))
}

get_agency_list <- function() {
  agency_list <- read_json(paste0(nextbus_json_feed_url, "agencyList"))
  agency_list$agency <- agency_list$agency %>%
    select(tag, title, short_title = shortTitle, region_title = regionTitle) %>%
    as_tibble()
  agency_list
}

get_route_list <- function(agency_tag) {
  route_list <- read_json(paste0(nextbus_json_feed_url, "routeList&a=", agency_tag))
  route_list$route <- route_list$route %>%
    select(tag, title) %>%
    as_tibble()
  route_list
}

get_route_config <- function(agency_tag, route_tag = NULL, exclude_path = FALSE) {
  url <- paste0(nextbus_json_feed_url, "routeConfig&a=", agency_tag)

  if (!is.null(route_tag)) {
    url <- paste0(url, "&r=", route_tag)
  }

  if (exclude_path) {
    url <- paste0(url, "&terse")
  }

  route_config <- read_json(url)

  r <- list(tag = route_config$route$tag,
            title = route_config$route$title)

  r$stop <- route_config$route$stop %>%
    select(tag, id = stopId, title, lat, lon) %>%
    mutate_at(vars(lat, lon), as.numeric) %>%
    as_tibble()

  if (!exclude_path) {
    r$path <- route_config$route$path$point %>%
      imap_dfr(~ mutate(.x, point = .y)) %>%
      select(point, lat, lon) %>%
      mutate_at(vars(lat, lon), as.numeric) %>%
      as_tibble()
  }

  directions <- NULL
  for (i in seq_along(route_config$route$direction$stop)) {
    direction <- cbind.data.frame(
      stop_tag = unname(route_config$route$direction$stop[[i]]),
      title = route_config$route$direction$title[i],
      use_for_ui = route_config$route$direction$useForUI[i],
      tag = route_config$route$direction$tag[i],
      name = route_config$route$direction$name[i]
    )
    directions <- rbind(directions, direction)
  }
  r$direction <- directions %>%
    mutate(use_for_ui = recode(use_for_ui, "true" = TRUE, "false" = FALSE)) %>%
    as_tibble()

  r$lat_limits <- as.numeric(c(route_config$route$latMin, route_config$route$latMax))
  r$lon_limits <- as.numeric(c(route_config$route$lonMin, route_config$route$lonMax))
  r$color <- paste0("#", route_config$route$color)
  r$opposite_color <- paste0("#", route_config$route$oppositeColor)
  r$copyright <- route_config$copyright
  r
}

get_predictions <- function(agency_tag, route_tag, stop_tag) {
  url <- paste0(nextbus_json_feed_url, "predictions&a=", agency_tag,
                "&r=", route_tag, "&s=", stop_tag)
  predictions <- read_json(url)
  predictions
}

get_predictions_for_multi_stops <- function(agency_tag, route_tags, stop_tags) {
  if (length(route_tags) == 1) {
    route_tags <- rep(route_tags, length(stop_tags))
  }
  stopifnot(length(route_tags) != length(stop_tags))
  url <- paste0(nextbus_json_feed_url, "predictionsForMultiStops&a=", agency_tag,"&r=", route_tag)
  for (i in seq_along(stop_tags)) {
    url <- paste0(url, "&stops=", route_tags[i], "|", stop_tags[i])
  }
  predictions <- read_json(url)
  predictions
}

get_schedule <- function(agency_tag, route_tag) {
  schedule <- read_json(paste0(nextbus_json_feed_url, "schedule&a=", agency_tag, "&r=", route_tag))
  schedule
}

get_messages <- function(agency_tag, route_tags) {
  url <- paste0(nextbus_json_feed_url, "messages&a=", agency_tag)
  for (route_tag in route_tags) {
    url <- paste0(url, "&r=", route_tag)
  }
  messages <- read_json(url)
  messages
}

get_vehicle_location <- function(agency_tag, vehicle_id) {
  vehicle_location <- read_json(paste0(nextbus_json_feed_url, "vehicleLocation&a=", agency_tag, "&v=", vehicle_id))
  vehicle_location
}

get_vehicle_locations <- function(agency_tag, route_tag, datetime = NULL) {
  if (is.null(datetime)) {
    epoch_time_in_msec = 0  # will provide data for the last 15 minutes
  } else {
    epoch_time_in_msec = ...
  }
  url <- paste0(nextbus_json_feed_url, "vehicleLocations&a=", agency_tag,
                "&r=", route_tag, "&t=", epoch_time_in_msec)
  vehicle_locations <- read_json(url)
  vehicle_locations$vehicle <- vehicle_locations$vehicle %>%
    as_tibble() %>%
    select(vehicle_id = id, route_tag = routeTag, lat, lon, predictable, speed_km_hr = speedKmHr,
           dir_tag = dirTag, heading, secs_since_report = secsSinceReport, leading_vehicle_id = leadingVehicleId) %>%
    mutate(predictable = recode(predictable, "true" = TRUE, "false" = FALSE)) %>%
    mutate_at(vars(lat, lon), as.numeric) %>%
    mutate_at(vars(speed_km_hr, heading, secs_since_report), as.integer)
  vehicle_locations
}

