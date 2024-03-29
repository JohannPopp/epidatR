
# Reproduce the R result of "SomeFakeData.epx"
someFake <- data.frame(status = factor(c("Verified", "Deleted", "Normal", "Normal")),
                       ID = 1:4,
                       date = strptime(c("2023-11-03", "2023-11-10", "2023-10-30", NA),
                                       format = "%Y-%m-%d", tz = "UTC"),
                       age = as.integer(c(65, NA, 18, 39)),
                       gender = factor(c("male", NA, "female", "male")),
                       height = as.integer(c(189, 160, NA, 170)),
                       weight = as.numeric(c(78, 52, 60, 81)),
                       notes = c("No notes", NA, NA, "This is fake data"))

study.info <- matrix(c("Anhand dieses Beispielprojektes erkläre ich das Arbeiten mit EpiData",
                       "Hamburg", "Beispielprojekt", "Eine Person", "Johann Popp",
                       "", "2019/05/21 20.20.55", "", "", "2019/05/21 20.20.55", "", "1"))
rownames(study.info) <-
  c("Abstract", "GeographicalCoverage", "Title", "UnitOfObservation", "Author", "Agency",
    "Created", "Identifier", "Keywords", "Modified", "Notes", "Version")
attributes(someFake)$study.info <- study.info
attributes(someFake)$variable.labels <- c("Record status", "Index", "Date", "Age (years)", "Gender", "Height (cm)", "Weight (kg)", "Notes")


# Test full conversion on a simple data set
test_that("Full-Conversion-SomeFake", {
  expect_equal(read.EpiData(epx.example("SomeFakeData.epx")),
               someFake)
})

# Test full conversion on a relational data set
# test_that("Full-Conversion-Clinical_Example", {
#   expect_equal(
#     paste(
#       unlist(read.EpiData(epx.example("Clinical_Example.epx"))),
#       collapse = ", "),
#     "1, 1, 123, 55, 2, 1, 1, 2, 30, 22, 58, 5, 22, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 30, 22, 58, 5, 22, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 30, 22, 58, 5, 22, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 1, 1, 123, 55, 2, 1, 0, 0, 0, 0, 0, 0, 20, 3, 6, 3, 67, 80, 4, 4, 200, 93, 0, 0, UTC, UTC, 0, 0, 1, 0, 1, 1, 26, 35, 59, 5, 22, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 26, 35, 59, 5, 22, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 26, 35, 59, 5, 22, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 1, 1, 123, 55, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 3, 1, 7, 6, 119, 119, 6, 1, 214, 181, 0, 0, UTC, UTC, 0, 0, 1, 2, NA, 3, 32, 8, 0, 6, 23, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 32, 8, 0, 6, 23, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 32, 8, 0, 6, 23, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 1, 1, 123, 55, 0, 0, 0, 0, 0, 0, 22, 22, 5, 5, 119, 119, 6, 6, 172, 172, 0, 0, UTC, UTC, 0, 0, 1, 2, NA, NA, 1, 1, 1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 37, 50, 1, 7, 23, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 37, 50, 1, 7, 23, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 37, 50, 1, 7, 23, 23, 28, 28, 10, 10, 123, 123, 2, 2, 331, 331, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 5, 5, 119, 119, 5, 5, 171, 171, 0, 0, UTC, UTC, 0, 0, 1, 1, 1, 123, 55, 55, 0, 0, 0, 0, 0, 0, 0, 0, 0, 22, 22, 22, 5, 5, 5, 119, 119, 119, 6, 6, 6, 172, 172, 172, 0, 0, 0, UTC, UTC, UTC, 0, 0, 0, 321, 9, 98, 15, 10, 8.9, 4.7, 3.3, 4.5, 7.1, 6.8, 7.2, 28, 31, 1, 2, 8, 9, 23, 23, 23, 28, 28, 28, 10, 10, 10, 123, 123, 123, 2, 2, 2, 331, 331, 331, 0, 0, 0, UTC, UTC, UTC, 0, 0, 0, 28, 31, 1, 2, 8, 9, 23, 23, 23, 28, 28, 28, 10, 10, 10, 123, 123, 123, 2, 2, 2, 331, 331, 331, 0, 0, 0, UTC, UTC, UTC, 0, 0, 0, 28, 31, 1, 2, 8, 9, 23, 23, 23, 28, 28, 28, 10, 10, 10, 123, 123, 123, 2, 2, 2, 331, 331, 331, 0, 0, 0, UTC, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 21, 5, 5, 5, 119, 119, 119, 5, 5, 5, 171, 171, 171, 0, 0, 0, UTC, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 21, 5, 5, 5, 119, 119, 119, 5, 5, 5, 171, 171, 171, 0, 0, 0, UTC, UTC, UTC, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 21, 21, 21, 5, 5, 5, 119, 119, 119, 5, 5, 5, 171, 171, 171, 0, 0, 0, UTC, UTC, UTC, 0, 0, 0, This data frame has no entries")
# })
#
#
#
