context("antaresDataTable")

describe("as.antaresDataTable", {
  it("converts data.frames in antaresDataTable", {
    mydata <- data.frame(area = "fr", timeId = 1, LOAD = 2)
    myAntaresData <- as.antaresDataTable(mydata, synthesis = TRUE, timeStep = "hourly", 
                                         type = "areas", opts = list())
    expect_is_antaresDataTable(myAntaresData)
  })
  
  it("does not change antaresDatatTables", {
    mydata <- data.frame(area = "fr", timeId = 1, LOAD = 2)
    myAntaresData <- as.antaresDataTable(mydata, synthesis = TRUE, timeStep = "hourly", 
                                         type = "areas", opts = list())
    expect_equal(myAntaresData, as.antaresDataTable(myAntaresData))
  })
  
  it("throws an error if x is of another type", {
    expect_error(as.antaresDataTable(1), "Cannot convert")
  })
})

describe("[.antaresDataTable", {
  mydata <- data.table(
    area = "fr",
    timeId = 1:5,
    LOAD = rexp(5),
    PROD = rexp(5)
  )
  mydata <- as.antaresDataTable(mydata, synthesis = TRUE, timeStep = "hourly", 
                                type = "areas", opts = list())
  
  it("filters an antaresDataTable", {
    expect_is_antaresDataTable(mydata[timeId < 3])
  })
  
  it("adds new column", {
    mydata[, balance := rnorm(5)]
    expect_is_antaresDataTable(mydata)
    expect_true("balance" %in% names(mydata))
  })
  
  it ("selects columns (all ID columns included)", {
    mydata <- mydata[, .(area, timeId, LOAD)]
    expect_is_antaresDataTable(mydata)
  })
  
  it("returns a 'normal' data.table if some ID column is missing", {
    summaryFR <- mydata[, .(LOAD = sum(LOAD)), by = area]
    expect_is(summaryFR, "data.table")
    expect_false(is(summaryFR, "antaresData"))
  })
})

describe("merge.antaresDataTable", {
  it ("returns an antaresDataTable", {
    mydata <- data.table(
      area = "fr",
      timeId = 1:5,
      LOAD = rexp(5),
      PROD = rexp(5)
    )
    mydata <- as.antaresDataTable(mydata, synthesis = TRUE, timeStep = "hourly", 
                                  type = "areas", opts = list())
    
    additionalData <- data.table(area = "fr", SOLAR = rexp(1))
    mergedData <- merge(mydata, additionalData, by = "area")
    
    expect_is_antaresDataTable(mergedData)
    expect_named(mergedData, c("area", "timeId", "LOAD", "PROD", "SOLAR"))
  })
})

describe("print.antaresDataTable", {
  
  mydata <- data.table(
    area = "fr",
    timeId = 1:5,
    LOAD = rexp(5),
    PROD = rexp(5)
  )
  mydata <- as.antaresDataTable(mydata, synthesis = TRUE, timeStep = "hourly", 
                                type = "areas", opts = list())
  
  it ("prints basic information", {
    expected_message <- sprintf("'antaresDataTable' object with dimension %s x %s",
                                nrow(mydata), ncol(mydata))
    expect_output(print(mydata), expected_message)
    expect_output(print(mydata), "Type: areas")
    expect_output(print(mydata), "Synthesis: TRUE")
  })
})
