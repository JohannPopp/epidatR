# Test replacement of defined missing values
test_that("Missings-SomeFake", {
  expect_equal(paste(
    unlist(
      suppressWarnings(epx.missing(
        epx.read(epx.extract(epx.example("SomeFakeData.epx"))$perDataSet$ds1),
        epx.extract(epx.example("SomeFakeData.epx"))$perDataSet$ds1))),
    collapse = ", "),
    "1, 2, 3, 4, 03/11/2023, 10/11/2023, 30/10/2023, NA, 65, NA, 18, 39, 2, NA, 1, 2, 189, 160, NA, 170, 78, 52, 60, 81, No notes, NA, NA, This is fake data")
})
