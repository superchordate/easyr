#' Get Rows with Duplicates
#' 
#' Pulls all rows with duplicates in a column, not just the duplicate row.
#' Author: Bryce Chamberlain.
#' 
#' @param x Data frame.
#' @param cols Column names as a character.
#' @param na Consider multiple NAs as duplicates?
#' 
#' @return Rows from the data frame in which the column is duplicated.
#'
#' @export
#' 
#' @examples 
#' ddt = bindf(cars, utils::head(cars, 10)) # create duplicated data.
#' drows(ddt, 'speed') # get the duplicated rows.
drows <- function(x, cols, na = FALSE) {

  # validate inputs  
  miscols = setdiff(cols, names(x))
  if(length(miscols) > 0) stop(glue::glue('Data missing columns: [{cc(miscols, sep = ", ")}]'))

  # get a matrix of values to check. matrices are faster than dataframes, rowSums in particular. 
  checkdt = as.matrix(dplyr::select_at(x, cols))
  checkdt = cbind(1:nrow(x), checkdt) # we need at least 2 columns, or checkdt = checkdt[-narows, ] will convert from matrix to vector.
  names(checkdt)[1] = 'row'

  # remove NAs.
  if(!na){
    narows = which(rowSums(is.na(checkdt[, -1, drop = FALSE])) > 0)  
    if(length(narows) > 0) checkdt = checkdt[-narows, ]
  }
  
  # get duplicates. 
  duprows = which(duplicated(checkdt[, -1]))
  if(length(duprows) == 0){
    cat('drows: No duplicates found.')
    return()
  }

  # return rows with duplicates.
  collapsed_vals = apply(checkdt[, -1, drop = FALSE], 1, paste, collapse = '')
  dvals = which(collapsed_vals %in% collapsed_vals[duprows])
  toreturn = checkdt[dvals, ]
  colnames(toreturn)[1] = 'row'

  toreturn = dplyr::arrange_at(dplyr::as_tibble(toreturn), cols)
  for(col in cols) class(toreturn[[col]]) = class(x[[col]])
  toreturn$row = as.integer(toreturn$row)
  
  return(toreturn)

}
