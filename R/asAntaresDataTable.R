#' Convert objects to antaresDataTable
#' 
#' @description 
#' This function converts a \code{data.frame} or a \code{data.table} into an
#' \code{antaresDataTable} object.
#' 
#' An \code{antaresDataTable} is simply a \code{data.table} with additional
#' attributes recording the time step, the type of data and the simulation 
#' options.
#'
#' @param x
#'   object to convert to a an \code{antaresDataList}.
#' @param ...
#'   Arguments to be passed to methods.
#'   
#' @return
#'   \code{antaresDataTable} object.
#' 
#' @export
as.antaresDataTable <- function(x, ...) {
  UseMethod("as.antaresDataTable", x)
}

#' @export
as.antaresDataTable.antaresDataTable <- function(x, ...) {
  x
}

#' @rdname as.antaresDataTable
#' 
#' @param synthesis
#'   Does the table contain synthetic results ?
#' @param timeStep
#'   Time step of the data. One of "hourly", "daily", "weekly", "monthly" or "annual".
#' @param type
#'   type of data: for instance "areas", "links", "clusters", etc.
#' @param opts
#'   Simulation options.
#' 
#' @method as.antaresDataTable data.table
#' @export
as.antaresDataTable.data.table <- function(x, synthesis, timeStep, type, opts = simOptions(), ...) {
  timeStep <- match.arg(timeStep, c("hourly", "daily", "weekly", "monthly", "annual"))
  
  setattr(x, "class", c("antaresDataTable", "antaresData", "data.table", "data.frame"))
  setattr(x, "type", type)
  setattr(x, "timeStep", timeStep)
  setattr(x, "synthesis", synthesis)
  setattr(x, "opts", opts)
  
  reorderCols(x)
  
  x
}

#' @export
as.antaresDataTable.data.frame <- function(x, ...) {
  x <- as.data.table(x)
  as.antaresDataTable(x, ...)
}

#' @export
as.antaresDataTable.default <- function(x, ...) {
  stop("Cannot convert this object to an 'antaresDataTable' object.")
}
