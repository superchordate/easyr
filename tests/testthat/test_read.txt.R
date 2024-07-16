test_that( "read text file", {

  expect_equal(
    read.txt( 
      test_file_( 'some-text.txt' )
    ),
    'This is some text 530.'
  )

})