test_that( 'basic hash works properly', {
  
  expect_equal(
    hashfiles( 
      test_file('null-columns.xlsx'),
      full.hash = TRUE
    ),
    'bd9ac3bb'
  )
  
})

test_that('missing files are handled properly', {
  
  expect_equal(
    hashfiles(c(test_file('null-columns.xlsx'), 'this-file-doesnt-exist.R'), skip.missing = TRUE, full.hash = TRUE),
    hashfiles(test_file('null-columns.xlsx'), full.hash = TRUE)
  )
  
  expect_error(
    hashfiles( c( test_file( 'null-columns.xlsx' ), 'this-file-doesnt-exist.R' ), full.hash = TRUE ),
    'File not found:'
  )
  
})

test_that( "handle entire folder", {
  expect_equal(
    hashfiles(test_file(''), full.hash = TRUE),
    'aa89a887'
  )
})

