# Test replacement of value codes with value labels
test_that("Labels-SomeFake", {
  expect_equal(paste(
    unlist(
      epx.labels(
        epx.read(epx.extract(epx.example("SomeFakeData.epx"))$perDataSet$ds1),
        epx.extract(epx.example("SomeFakeData.epx"))$perDataSet$ds1)),
    collapse = ", "),
    "1, 2, 3, 4, 03/11/2023, 10/11/2023, 30/10/2023, NA, 65, no data, 18, 39, male, no data, female, male, 189, 160, no data, 170, 78, 52, 60, 81, No notes, NA, NA, This is fake data")
})
