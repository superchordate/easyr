# Get test file path.
# Usage:
#   test_file_("blanks.xls")
# Modified from https://github.com/tidyverse/readxl/blob/master/tests/testthat/helper.R
test_file_ = function( fname ) paste0(c(rprojroot::find_testthat_root_file(), "test-files", fname), collapse = '/')
