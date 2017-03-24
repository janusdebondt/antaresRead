#' @export
print.antaresDataList <- function(x, ...) {
  elements <- names(x)
  N <- length(elements)
  if (N == 1) mytext <- paste("element", elements)
  else {
    mytext <- paste("elements", paste(elements[-N], collapse = ", "), "and", elements[N])
  }
  
  cat(sprintf("'antaresDataList' object with %s\n",mytext))
  cat(sprintf("TimeStep: %s\nSynthesis: %s\n",
              attr(x, "type"), attr(x, "timeStep"), attr(x, "synthesis")))
  
  # Overwrite print.antaresDataTable so that informations like synthesis and 
  # timestep are not printed again.
  print.antaresDataTable <- function(x, ...) NextMethod()
  
  for (n in names(x)) {
    cat(sprintf("\n.$%s (%s x %s)\n", n, nrow(x[[n]]), ncol(x[[n]])))
    print(x[[n]])
  }
}
