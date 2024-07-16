test_that('hashfiles', {
  
  # single file. 
  expect_equal(
    hashfiles(test_file_('null-columns.xlsx'), full.hash = TRUE),
    'bd9ac3bb'
  )
  expect_equal(
    hashfiles(test_file_('null-columns.xlsx'), full.hash = FALSE),
    '72158de1'
  )
  
  # skip missing files. 
  expect_equal(
    hashfiles(c(test_file_('null-columns.xlsx'), 'this-file-doesnt-exist.R'), skip.missing = TRUE, full.hash = TRUE),
    hashfiles(test_file_('null-columns.xlsx'), full.hash = TRUE)
  )  
  expect_error(
    hashfiles( c( test_file_( 'null-columns.xlsx' ), 'this-file-doesnt-exist.R' ), full.hash = TRUE ),
    'File not found:'
  )

  # hash folder.
  expect_equal(
    hashfiles(test_file_(''), full.hash = TRUE),
    '50273b11'
  )
  expect_equal(
    hashfiles(test_file_(''), full.hash = FALSE),
    'ab968fc0'
  )
  
  # ensure hash changes when a new file is added. 
  tempfile = test_file_('temp-cars.csv')
  write.csv(cars, tempfile)
  expect_true(
    hashfiles(test_file_(''), full.hash = TRUE) != '50273b11'
  )
  expect_true(
    hashfiles(test_file_(''), full.hash = FALSE) != 'ab968fc0'
  )
  file.remove(tempfile)

})

