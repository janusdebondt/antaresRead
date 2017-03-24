context("antaresDataList")

describe("as.antaresDataList", {
  it("converts data.frames", {
    dt <- data.frame(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataList(dt, TRUE, "hourly", "areas", opts = list())
    expect_is_antaresDataList(adt)
    expect_named(adt, "areas")
  })
  
  it("converts data.tables", {
    dt <- data.table(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataList(dt, TRUE, "hourly", "areas", opts = list())
    expect_is_antaresDataList(adt)
    expect_named(adt, "areas")
  })
  
  it("converts antaresDataTables", {
    dt <- data.table(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataTable(dt, TRUE, "hourly", "areas", opts = list())
    adl <- as.antaresDataList(adt)
    expect_is_antaresDataList(adl)
    expect_named(adl, "areas")
  })
  
  it("converts lists of tables", {
    l <- list(areas = data.table(timeId = 1:10, area = "a", value = rnorm(10)))
    adl <- as.antaresDataList(l, TRUE, "hourly", opts = list())
    expect_is_antaresDataList(adl)
    expect_named(adl, "areas")
  })
  
  it("does not change antaresDataLists", {
    dt <- data.table(timeId = 1:10, area = "a", value = rnorm(10))
    adt <- as.antaresDataList(dt, TRUE, "hourly", "areas", opts = list())
    expect_identical(adt, as.antaresDataList(adt))
  })
  
  it("throws an error if x is of another type", {
    expect_error(as.antaresDataList(1), "Cannot convert")
  })
})
