# Copyright © 2016 RTE Réseau de transport d’électricité

# When doing some simple modifications on an antaresDataTable, attributes are
# lost. This is why we create a specific "[" method.
#' @export
"[.antaresDataTable" <- function(x, ...) {
  attrs <- attributes(x)
  idCols <- idCols(x, exclude = c("time", "day", "week", "month", "hour"))
  
  x <- NextMethod("[", x)
  
  if (inherits(x, "data.table")) {   
    setattr(x, "class", c("data.table", "data.frame"))
    if (all(idCols %in% names(x))) {
      x <- as.antaresDataTable(x, synthesis = attrs$synthesis, 
                               timeStep = attrs$timeStep, opts = attrs$opts,
                               type = attrs$type)
    }
  }
  
  x
}

#' @export
merge.antaresDataTable <- function(x, ...) {
  attrs <- attributes(x)
  idCols <- idCols(x)
  
  x <- NextMethod(merge, x)
  
  x <- as.antaresDataTable(x, synthesis = attrs$synthesis, 
                           timeStep = attrs$timeStep, opts = attrs$opts,
                           type = attrs$type)
}

#' @export
print.antaresDataTable <- function(x, ...) {
  cat(sprintf("'antaresDataTable' object with dimension %s x %s\n", nrow(x), ncol(x)))
  cat(sprintf("Type: %s\nTimeStep: %s\nSynthesis: %s\n",
              attr(x, "type"), attr(x, "timeStep"), attr(x, "synthesis")))
  NextMethod()
}
