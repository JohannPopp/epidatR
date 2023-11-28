# Test the transformation into a data.frame
test_that("data.frame-SomeFake", {
  expect_equal(
    paste(
      unlist(epx.read(epx.extract(epx.example("SomeFakeData.epx"))$perDataSet$ds1)),
      collapse = ", "),
    "1, 2, 3, 4, 03/11/2023, 10/11/2023, 30/10/2023, NA, 65, -1, 18, 39, 2, -1, 1, 2, 189, 160, -1, 170, 78, 52, 60, 81, No notes, NA, NA, This is fake data")
})
