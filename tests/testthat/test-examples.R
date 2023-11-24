test_that("Clinical example", {
  expect_equal(digest::digest(read.EpiData(epx.example("Clinical_Example.epx")), "sha256"),
               "dbe4b3e74bb408ebe0c6be106a7d7d296b8a11c11003b8ddaf92818871f16074")
})


test_that("One line dataframes", {
  expect_equal(digest::digest(read.EpiData(epx.example("OneLine.epx")), "sha256"),
               "109f73215998a2206395b116863e8b6044ad8077e2e25ead5030c99f47dfcdd1")
})

test_that("sample.v3.epx", {
  expect_equal(digest::digest(read.EpiData(epx.example("sample.v3.epx")), "sha256"),
               "b28f1ecb88cd4619c3cfcdb44eab7f6d78eaf605578162da1b2ee5d79ef4c3c9")
})

test_that("Marathon", {
  expect_equal(digest::digest(read.EpiData(epx.example("marathon.epx")), "sha256"),
               "6a9f5333c3eb91a96793ce9ebccea66a87c730b76fa3e935cb5c64e792bcf1e4")
})
