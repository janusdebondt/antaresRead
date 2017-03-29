# thermalAvailability of clusters is a special case.
# Series are not directly available in the output: only the ids of the series used
# for each Monte Carlo scenario are aviable. We read these and reconstruct the
# good time series.
.importThermal <- function(areas, synthesis, timeStep, mcYears, opts, showProgress, parallel, ...) {
  if (showProgress) cat("Importing thermalAvailabilties\n")
  
  areas <- intersect(areas, opts$areasWithClusters)
  if (length(areas) == 0) return(NULL)
  if (synthesis) mcYears <- opts$mcYears
  
  res <- llply(areas, .importThermalForArea,
               synthesis = synthesis, timeStep = timeStep, mcYears = mcYears, opts = opts,
               .parallel = parallel,
               .paropts = list(.packages="antaresRead"),
               .progress = ifelse(showProgress, "text", "none")) 
  
  res <- rbindlist(res)
  
  # Compute the number of available units
  clusterDesc <- readClusterDesc(opts)
  res <- merge(res, clusterDesc[, .(area, cluster, nominalcapacity)],
                  by = c("area", "cluster"))
  .mergeByRef(res, clusterDesc, on = c("area", "cluster"), "nominalcapacity")
  
  res[, availableUnits :=  ceiling(thermalAvailability / nominalcapacity)]
  res[, nominalcapacity := NULL]
  
  res <- as.antaresDataTable(res, synthesis = FALSE, timeStep = "hourly", 
                             type = "thermalAvailability", opts = opts)
  res <- changeTimeStep(res, timeStep)
  
  if (synthesis) {
    res <- res[, .(thermalAvailability=mean(thermalAvailability),
                   availableUnits = mean(availableUnits)), 
               keyby = .(area, cluster, timeId)]
  }
  
  res
}


.importThermalForArea <- function(area, synthesis, timeStep, mcYears, opts, ...) {
  
  
  # Path to the files containing the IDs of the time series used for each
  # Monte-Carlo years.
  pathTSNumbers <- file.path(opts$simPath, "ts-numbers/thermal")
  
  # Path to the time series. Ideally, time series are stored in output. If it is
  # not the case, read the series in the input.
  pathInput <- file.path(opts$simPath, "ts-generator/thermal/mc-0")
  
  if (dir.exists(pathInput)) {
    filePattern <- sprintf("%s/%s/%%s.txt", pathInput, area)
  } else {
    pathInput <- file.path(opts$inputPath, "thermal/series")
    filePattern <- sprintf("%s/%s/%%s/series.txt", pathInput, area)
  }
  
  # Read the Ids of the time series used in each Monte-Carlo Scenario.
  cls <- list.files(file.path(pathTSNumbers, area))
  if (length(cls) == 0) return(NULL)
  
  nameCls <- gsub(".txt", "", cls)
  
  tsIds <- llply(cls, function(cl) {
    as.numeric(readLines(file.path(pathTSNumbers, area, cl))[-1])
  })
  
  names(tsIds) <- nameCls
  
  # Two nested loops: clusters, Monte Carlo simulations.
  series <- ldply(nameCls, function(cl) {
    ids <- tsIds[[cl]][mcYears]
    colToRead <- sort(unique(ids)) # Columns to read in the ts file
    colIds <- sapply(ids, function(i) which(colToRead == i)) # correspondance between the initial ids and the columns in the generated table
    
    ts <- fread(sprintf(filePattern, cl), integer64 = "numeric", select = colToRead)
    
    ldply(1:length(ids), function(i) {
      data.frame(
        area = area, 
        cluster = cl, 
        mcYear = mcYears[i],
        timeId = 1:nrow(ts),
        thermalAvailability = ts[[ colIds[i] ]]
      )
    })
  })
  
  series <- data.table(series)
  
  series <- series[timeId %in% opts$timeIdMin:opts$timeIdMax]
  
  series
}
