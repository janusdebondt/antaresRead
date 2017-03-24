expect_is_antaresDataTable <- function(x) {
  expect_is(x, "data.table")
  expect_is(x, "antaresDataTable")
  expect_true("timeId" %in% names(x))
  expect_attr(x, "timeStep")
  expect_attr(x, "synthesis")
  expect_attr(x, "type")
  expect_attr(x, "opts")
}

expect_is_antaresDataList <- function(x) {
  expect_is(x, "list")
  expect_is(x, "antaresDataList")
  expect_attr(x, "timeStep")
  expect_attr(x, "synthesis")
  expect_attr(x, "opts")
  for (i in seq_along(x)) {
    expect_is_antaresDataTable(x[[i]])
  }
}