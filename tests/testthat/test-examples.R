test_that("Clinical example", {
  expect_equal(digest::digest(read.EpiData(epidatR.example("Clinical_Example.epx")), "sha256"),
               "bbe783be07fee596680fc4e192b38cebdb2530b097b453cb96b706369f7a2949")
})

test_that("Beispielprojekt", {
  expect_equal(digest::digest(read.EpiData(epidatR.example("Beispielprojekt.epx")), "sha256"),
               "4a256824e75a33ba80d447f86791354fb1a59595179861bf439d12c5d5cab9d6")
})

test_that("One line dataframes", {
  expect_equal(digest::digest(read.EpiData(epidatR.example("OneLine.epx")), "sha256"),
               "8e240fe82ed216461cda493346718831c89b00356b7189b28b377800fc73362d")
})

test_that("sample.v3.epx", {
  expect_equal(digest::digest(read.EpiData(epidatR.example("sample.v3.epx")), "sha256"),
               "8e18a82aa1caa437d48f0259b95c64337ac92ba8265df9f60b839a25013543ff")
})

test_that("Marathon", {
  expect_equal(digest::digest(read.EpiData(epidatR.example("marathon.epx")), "sha256"),
               "4929d1326572087861c63be3a900bd8523439851ae38642ea515287f9c15805f")
})
