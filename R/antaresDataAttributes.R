#' Get and set attributes of antaresData objects
#' 
#' These functions permit to get and set attributes of an `antaresData` object.
#' Four attribtues can be accessed and modified: time step, synthesis, type of
#' data (areas, links, etc.) and simulation options.
#'
#' @param x Object of class `antaresData`. For `type`, it can only  be an
#'   `antaresDataTable` object.
#' @param value New value of the attribute.
#' 
#' @return Getter functions return the value of the corresponding attribute.
#' Setter functions are used for their side effect.
#' 
#' @examples
#' require(data.table)
#' 
#' mydata <- data.table(area = "fr", timeId = 1:5, LOAD = 1:5)
#' mydata <- as.antaresDataTable(mydata, synthesis = TRUE, timeStep = "hourly",
#'                               type = "areas", opts = list())
#' 
#' timeStep(mydata)
#' 
#' # Modify timeStep
#' timeStep(mydata) <- "daily"
#' timeStep(mydata)
#' 
#' # It also works on antaresDataList objects
#' mydata <- as.antaresDataList(mydata)
#' 
#' timeStep(mydata) <- "weekly"
#' timeStep(mydata)
#' timeStep(mydata$areas)
#' 
#' @export
timeStep <- function(x) {
  assert_that(inherits(x, "antaresData"))
  attr(x, "timeStep")
}

#' @export
#' @rdname timeStep
setTimeStep <- function(x, value=c("hourly", "daily", "weekly", "monthly", "annual")) {
  assert_that(inherits(x, "antaresData"))
  value <- match.arg(value)
  if (is(x, "antaresDataList")) {
    for (n in names(x)) {
      setTimeStep(x[[n]], value)
    }
  }
  
  setattr(x, "timeStep", value)
}

#' @export
#' @rdname timeStep
synthesis <- function(x) {
  assert_that(inherits(x, "antaresData"))
  attr(x, "synthesis")
}

#' @export
#' @rdname timeStep
`synthesis<-` <- function(x, value) {
  assert_that(inherits(x, "antaresData"))
  assert_that(is.flag(value))
  
  if (is(x, "antaresDataList")) {
    for (n in names(x)) {
      synthesis(x[[n]]) <- value
    }
  }
  
  setattr(x, "synthesis", value)
}

#' @export
#' @rdname timeStep
type <- function(x) {
  assert_that(inherits(x, "antaresDataTable"))
  attr(x, "type")
}

#' @export
#' @rdname timeStep
`type<-` <- function(x, value) {
  assert_that(inherits(x, "antaresDataTable"))
  assert_that(is.string(value))
  setattr(x, "type", value)
}

#' @export
#' @rdname timeStep
simOptions <- function(x = NULL) {
  if (is.null(x)) {
    opts <- getOption("antares")
    if (is.null(opts)) stop("Default antares options are not set.")
    else return(opts)
  }
  assert_that(inherits(x, "antaresData"))
  attr(x, "opts")
}

#' @export
#' @rdname timeStep
`simOptions<-` <- function(x, value) {
  assert_that(inherits(x, "antaresData"))
  
  if (is(x, "antaresDataList")) {
    for (n in names(x)) {
      simOptions(x[[n]]) <- value
    }
  }
  
  setattr(x, "opts", value)
}
