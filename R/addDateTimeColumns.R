# Copyright © 2016 RTE Réseau de transport d’électricité

#' Add date time columns to an antaresData object
#' 
#' This function adds date time columns to an `antaresData` object. It adds a 
#' time column whose content depends on the time step of the data. It also adds 
#' other columns depending on the time step. 
#' 
#' @param x An `antaresData` object.
#' 
#' @return 
#' The function modifies its input by reference. For convenience it returns the
#' modified input.
#' 
#' @examples
#' opts <- list(
#'   start = as.POSIXlt("2017-01-01", tz = "UTC"),
#'   firstWeekday = "Monday"
#' )
#' 
#' sourcedata <- data.frame(area = "fr", timeId = 1:5, LOAD = rexp(5))
#' mydata <- as.antaresDataTable(sourcedata, timeStep = "hourly", synthesis = TRUE,
#'                               type = "areas", opts = opts)
#' 
#' addDateTimeColumns(mydata)
#' mydata
#' 
#' # Same except we use a daily time
#' mydata <- as.antaresDataTable(sourcedata, timeStep = "daily", synthesis = TRUE,
#'                               type = "areas", opts = opts)
#' addDateTimeColumns(mydata)
#' mydata
#' @export
addDateTimeColumns <- function(x) {
  assert_that(inherits(x, "antaresData"))
  
  timeStep <- timeStep(x)
  opts <- simOptions(x)
  
  if (is(x, "antaresDataList")) {
    for (el in x) addDateTimeColumns(el)
    return(invisible(x))
  }
  
  # If table is empty, return it as is
  if (nrow(x) == 0) {
    return(x)
  }
  
  if (timeStep == "hourly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*365) - 1
    
    newCols <- data.table(time = timestamp,
                          day = lubridate::day(timestamp),
                          month = as.factor(toupper(lubridate::month(timestamp, TRUE, TRUE))),
                          hour = as.factor(format(timestamp, format = "%H:%M")))
    
  } else if (timeStep == "daily") {
    
    date <- as.Date(opts$start)
    date <- date + 1:365 - 1
    
    newCols <- data.table(time = date,
                          day = lubridate::day(date),
                          month = as.factor(toupper(lubridate::month(date, TRUE, TRUE))))
    
  } else if (timeStep == "weekly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*365) - 1
    
    weekId <- convertTimeId(1:(24*365), "weekly", opts)
    date <- as.Date(tapply(timestamp, weekId, function(x) as.Date(min(x))), origin = "1970-1-1")
    date[1] <- date[2] - 7
    
    week <- format(date, format = "%G-w%V")
    
    newCols <- data.table(time = as.factor(week))
    
  } else if (timeStep == "monthly") {
    
    timestamp <- as.POSIXct(opts$start)
    lubridate::hour(timestamp) <- lubridate::hour(timestamp) + 1:(24*365) - 1
    
    monthId <- convertTimeId(1:(24*365), "monthly", opts)
    month <- tapply(timestamp, monthId, function(x) format(min(x), format = "%Y-%m"))
    
    monthName <- tapply(timestamp, monthId, function(x) toupper(lubridate::month(min(x), TRUE, TRUE)))
    
    newCols <- data.table(time = month, 
                          month = as.factor(toupper(monthName)))
    
  } else {
    
    year <- lubridate::year(as.POSIXct(opts$start))
    
    newCols <- data.table(time = year)
    
  }
  
  newCols$timeId <- seq_len(nrow(newCols))
  mergeByRef(x, newCols, by = "timeId")
  reorderCols(x)
  
  invisible(x)
}
