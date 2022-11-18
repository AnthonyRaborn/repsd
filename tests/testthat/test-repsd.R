test_that("repsd returns list of length 4", {
  expect_equal(4, length(repsd()))
})

test_that("repsd repsd_each_item defaults return 20 repsd with specified values", {
  test_repsd_output =
    repsd()

  expect_equal(20, length(test_repsd_output$repsd_each_item))
  expect_equal(canon_repsd_output$repsd_each_item, test_repsd_output$repsd_each_item)
})

test_that('repsd returns the correct number of removed observation', {
  test_repsd_output =
    repsd()

  expect_equal(canon_repsd_output$total_focal_removed, test_repsd_output$total_focal_removed)
  expect_equal(canon_repsd_output$total_non_focal_removed, test_repsd_output$total_non_focal_removed)
  expect_equal(canon_repsd_output$num_removed_over_foc_max, test_repsd_output$num_removed_over_foc_max)
})
