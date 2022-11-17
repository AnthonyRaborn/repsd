test_that("repsd returns list of length 4", {
  expect_equal(4, length(repsd()))
})

test_that("repsd repsd_each_item defaults return 20 repsd with specified values", {
  test_repsd_output =
    repsd()

  expect_equal(20, length(test_repsd_output$repsd_each_item))
})
