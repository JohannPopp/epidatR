
test_that("Test XML scheme", {
  expect_match(epx.extract(epx.example("SomeFakeData.epx"))$infoEpiData[[1]][1],
               "http://www.epidata.dk/XML/2.1 http://www.epidata.dk/XML/2.1/epx.xsd")
})

