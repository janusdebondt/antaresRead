# Hydro storage is a special case: only input time serires are available either
# in the output folder or in the input folder depending on the choices of the
# Antares user.
# The ID of the time series used for each Monte-Carlo scenario need to be 
# retrieved from the output folder.
# Another difficulty is that time series is only available at monthly time step.
#
.importHydroStorage  <- function(areas, synthesis, timeStep, mcYears, showProgress, opts, parallel, ...) {
  if (showProgress) cat("Importing hydroStorage\n")
  if (synthesis) mcYears <- opts$mcYears
  res <- llply(areas, .importHydroStorageForArea, synthesis = synthesis, 
               timeStep = timeStep, mcYears = mcYears, opts = opts,
               .parallel = parallel, .progress = ifelse(showProgress, "text", "none")
  )
  res <- rbindlist(res)
  res <- as.antaresDataTable(res, synthesis = FALSE, timeStep = "monthly", 
                             type = "hydroStorage", opts = opts)
  res <- changeTimeStep(res, timeStep, opts = opts)
  if (synthesis) {
    res <- res[, .(hydroStorage=mean(hydroStorage)), keyby = .(area, timeId)]
  }
  res
}

.importHydroStorageForArea <- function(area, synthesis, timeStep, mcYears, opts, ...) {
  pathTSNumbers <- file.path(opts$simPath, "ts-numbers/hydro")
  
  
  # Read the Ids of the time series used in each Monte-Carlo Scenario.
  tsIds <- as.numeric(readLines(file.path(pathTSNumbers, paste0(area, ".txt")))[-1])
  tsIds <- tsIds[mcYears]
  
  # Input time series
  pathInput <- file.path(opts$simPath, "ts-generator/hydro/mc-0")
  
  if (dir.exists(pathInput)) {
    f <- file.path(pathInput, area, "storage.txt")
  } else {
    pathInput <- file.path(opts$inputPath, "hydro/series")
    f <- file.path(pathInput, area, "mod.txt")
  }
  
  timeRange <- range(convertTimeId(opts$timeIdMin:opts$timeIdMax, "monthly", opts))
  
  if (file.size(f) == 0) {
    series <- ldply(1:length(tsIds), function(i) {
      data.frame(
        area = area, 
        mcYear = mcYears[i],
        timeId = timeRange[1]:timeRange[2],
        hydroStorage = rep(0L, length(timeRange[1]:timeRange[2]))
      )
    })
  } else {
    colToRead <- sort(unique(tsIds)) # Columns to read in the ts file
    colIds <- sapply(tsIds, function(i) which(colToRead == i)) # link between the initial ids and the columns in the generated table
    
    ts <- fread(f, integer64 = "numeric", select = colToRead)
    ts <- ts[timeRange[1]:timeRange[2]]
    
    series <- ldply(1:length(tsIds), function(i) {
      data.frame(
        area = area, 
        mcYear = mcYears[i],
        timeId = timeRange[1]:timeRange[2],
        hydroStorage = ts[[ colIds[i] ]]
      )
    })
  }
  
  data.table(series)
}
