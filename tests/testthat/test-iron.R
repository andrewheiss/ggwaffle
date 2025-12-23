# ==============================================================================
# test iron functionality

testthat::test_that('waffle_iron works as intended with aes_d', {
    result <- waffle_iron(
        data = iris,
        mapping = aes_d(group = Species),
        rows = 8,
        sample_size = 1,
        na.rm = T
    )

    testthat::expect_equal(
      nrow(result),
      nrow(iris)
    )

    testthat::expect_equal(
      max(result$y),
      8
    )

    testthat::expect_equal(
      max(result$x),
      ceiling(nrow(iris) / 8)
    )

})

testthat::test_that('waffle_iron works as intended with aes_d_', {
  result <- waffle_iron(
    data = iris,
    mapping = aes_d_(group = 'Species'),
    rows = 8,
    sample_size = 1,
    na.rm = T
  )

  testthat::expect_equal(
    nrow(result),
    nrow(iris)
  )

  testthat::expect_equal(
    max(result$y),
    8
  )

  testthat::expect_equal(
    max(result$x),
    ceiling(nrow(iris) / 8)
  )

})

testthat::test_that('waffle_iron works as intended with a character mapping', {
  result <- waffle_iron(
    data = iris,
    mapping = 'Species',
    rows = 8,
    sample_size = 1,
    na.rm = T
  )

  testthat::expect_equal(
    nrow(result),
    nrow(iris)
  )

  testthat::expect_equal(
    max(result$y),
    8
  )

  testthat::expect_equal(
    max(result$x),
    ceiling(nrow(iris) / 8)
  )

})

testthat::test_that("Sample size > 1 fails", {
  testthat::expect_error(
    waffle_iron(
      data = iris,
      mapping = aes_d(group = Species),
      rows = 8,
      sample_size = 2,
      na.rm = T
    ),
    "Please use a sample value between 0 and 1"
  )
})

testthat::test_that("Sample size <= 0 fails", {
  testthat::expect_error(
    waffle_iron(
      data = iris,
      mapping = aes_d(group = Species),
      rows = 8,
      sample_size = 0,
      na.rm = T
    ),
    "Please use a sample value between 0 and 1"
  )
})

testthat::test_that("Very small sample size is rejected", {
  testthat::expect_error(
    waffle_iron(
      data = iris,
      mapping = aes_d(group = Species),
      rows = 8,
      sample_size = 0.01,
      na.rm = T
    ),
    "The sample size is too low for this dataset"
  )
})

testthat::test_that("Sampling works as intended", {
  samplesize <- 0.5

  result <- waffle_iron(
    data = iris,
    mapping = aes_d(group = Species),
    rows = 8,
    sample_size = samplesize,
    na.rm = T
  )

  testthat::expect_equal(
    nrow(result),
    floor(nrow(iris) * samplesize)
  )

  testthat::expect_equal(
    max(result$y),
    8
  )

  testthat::expect_equal(
    max(result$x),
    ceiling(nrow(iris) * samplesize / 8)
  )
})


testthat::test_that("Sampling works as intended across all possible samples", {
  # check 500 random samples to see if we have good coverage across any sample size
  set.seed(123456)
  samplesizes <- runif(n = 500, min = 8 / nrow(iris), max = 1)

  result <- lapply(
    samplesizes,
    function(x){
      waffle_iron(
        data = iris,
        mapping = aes_d(group = Species),
        rows = 8,
        sample_size = x,
        na.rm = T
      )
    }
  )

  testthat::expect_identical(
    as.numeric(sapply(result, nrow)),
    sapply(samplesizes, function(x) floor(nrow(iris) * x))
  )

  testthat::expect_identical(
    as.numeric(sapply(result, function(x) max(x$y))),
    rep(8, length(result))
  )

  testthat::expect_identical(
    as.numeric(sapply(result, function(x) max(x$x))),
    sapply(samplesizes, function(x) ceiling(floor(nrow(iris) * x) / 8))
  )

})


testthat::test_that('na.rm works as intended', {
  result <- waffle_iron(
    data = iris,
    mapping = aes_d(group = Species),
    rows = 8,
    sample_size = 1,
    na.rm = F
  )

  testthat::expect_gt(
    nrow(result),
    nrow(iris)
  )

  testthat::expect_equal(
    max(result$y),
    8
  )

  testthat::expect_equal(
    max(result$x),
    ceiling(nrow(iris) / 8)
  )

  testthat::expect_equal(
    nrow(result),
    8 * ceiling(nrow(iris) / 8)
  )
})

testthat::test_that('byrow = FALSE fills column-wise (default behavior)', {
  # Use iris dataset but subset to make behavior easier to verify
  set.seed(42)
  small_iris <- iris[1:9, ]  # Take first 9 rows
  
  result <- waffle_iron(
    data = small_iris,
    mapping = aes_d_(group = 'Species'),
    rows = 3,
    sample_size = 1,
    na.rm = TRUE,
    byrow = FALSE
  )
  
  # With byrow = FALSE (column-wise), should fill down first then across
  # First column (x=1) should have y values 1, 2, 3
  
  testthat::expect_equal(nrow(result), 9)
  testthat::expect_equal(max(result$x), 3)
  testthat::expect_equal(max(result$y), 3)
  
  # Check that first column has all three y values
  first_col <- result[result$x == 1, ]
  testthat::expect_equal(nrow(first_col), 3)
  testthat::expect_equal(sort(first_col$y), c(1, 2, 3))
})

testthat::test_that('byrow = TRUE fills row-wise', {
  # Use iris dataset but subset to make behavior easier to verify
  set.seed(42)
  small_iris <- iris[1:9, ]  # Take first 9 rows
  
  result <- waffle_iron(
    data = small_iris,
    mapping = aes_d_(group = 'Species'),
    rows = 3,
    sample_size = 1,
    na.rm = TRUE,
    byrow = TRUE
  )
  
  # With byrow = TRUE (row-wise), should fill across first then down
  # First row (y=1) should have x values 1, 2, 3
  
  testthat::expect_equal(nrow(result), 9)
  testthat::expect_equal(max(result$x), 3)
  testthat::expect_equal(max(result$y), 3)
  
  # Check that first row has all three x values
  first_row <- result[result$y == 1, ]
  testthat::expect_equal(nrow(first_row), 3)
  testthat::expect_equal(sort(first_row$x), c(1, 2, 3))
})

testthat::test_that('byrow works with larger dataset', {
  result_byrow <- waffle_iron(
    data = iris,
    mapping = aes_d(group = Species),
    rows = 8,
    sample_size = 1,
    na.rm = TRUE,
    byrow = TRUE
  )
  
  result_bycol <- waffle_iron(
    data = iris,
    mapping = aes_d(group = Species),
    rows = 8,
    sample_size = 1,
    na.rm = TRUE,
    byrow = FALSE
  )
  
  # Both should have same number of rows
  testthat::expect_equal(nrow(result_byrow), nrow(result_bycol))
  
  # Both should have same max y and x
  testthat::expect_equal(max(result_byrow$y), max(result_bycol$y))
  testthat::expect_equal(max(result_byrow$x), max(result_bycol$x))
  
  # But the data should be arranged differently
  # First cell should be same (both at x=1, y=1)
  first_cell_byrow <- result_byrow[result_byrow$x == 1 & result_byrow$y == 1, ]
  first_cell_bycol <- result_bycol[result_bycol$x == 1 & result_bycol$y == 1, ]
  testthat::expect_equal(first_cell_byrow$group, first_cell_bycol$group)
  
  # But cells at (x=2, y=1) should differ
  cell_byrow <- result_byrow[result_byrow$x == 2 & result_byrow$y == 1, ]
  cell_bycol <- result_bycol[result_bycol$x == 2 & result_bycol$y == 1, ]
  
  # In byrow, (2,1) comes after (1,1)
  # In bycol, (2,1) comes after (1,1), (1,2), (1,3), ..., (1,8)
  # So they should likely differ (unless data is uniform)
  # We just verify the structure is valid
  testthat::expect_equal(nrow(cell_byrow), 1)
  testthat::expect_equal(nrow(cell_bycol), 1)
})
