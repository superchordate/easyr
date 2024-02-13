test_that("works as expected", {

  # Duplicate the cars data.
  ddt = dplyr::bind_rows(cars, cars)
  
  # Add NA rows.
  for(i in c(4, 6, 34, 15)){
    ddt$speed[i] = NA
    ddt$dist[i] = NA
  }
  
  expect_equal(nrow(drows(ddt, 'speed')), 95)
  expect_equal(nrow(drows(ddt, 'speed', na = TRUE)), 95 + 4)

  dt = tibble(read.delim(text = 'col1	col2	col3
a	1	x
a	1	y
b	2	z
a		x
a		y
'))
  expect = tibble(read.delim(text = 'col1	col2
a	1
a	1
'))
  expect_equal(drows(dt, c('col1', 'col2')), expect)
  
  expect = tibble(read.delim(text = 'col1	col2
a	1
a	1
a	
a	
'))
  expect_equal(drows(dt, c('col1', 'col2'), na = TRUE), expect)

})