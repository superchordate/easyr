#' Get Rows with Duplicates
#' 
#' Pulls all rows with duplicates in a column, not just the duplicate row.
#' Author: Bryce Chamberlain.
#' 
#' @param x Data frame.
#' @param c Column names as a character.
#' @param na Consider multiple NAs as duplicates?
#' 
#' @return Rows from the data frame in which the column is duplicated.
#'
#' @export
#' 
#' @examples 
#' ddt = bindf(cars, utils::head(cars, 10)) # create duplicated data.
#' drows(ddt, 'speed') # get the duplicated rows.
drows <- function(x, c, na = FALSE) {

  # validate inputs  
  miscols = setdiff(c, names(x))
  if(length(miscols) > 0) stop(glue::glue('Data missing columns: [{cc(miscols, sep = ", ")}]'))

  # get a matrix of values to check. matrices are faster than dataframes, rowSums in particular. 
  checkdt = as.matrix(dplyr::select_at(x, c))
  checkdt = cbind(checkdt, 'dummy') # we need at least 2 columns, or checkdt = checkdt[-narows, ] will convert from matrix to vector.

  # remove NAs.
  if(!na){
    narows = which(rowSums(is.na(checkdt)) > 0)  
    if(length(narows) > 0) checkdt = checkdt[-narows, ]
  }
  
  # get duplicates. 
  duprows = which(duplicated(checkdt))
  if(length(duprows) == 0){
    cat('drows: No duplicates found.')
    return()
  }

  # return rows with duplicates.
  dvals = which(paste0(checkdt) %in% paste0(checkdt[duprows]))
  toreturn = checkdt[dvals, ]
  colnames(toreturn) = c(c, 'dummy')

  toreturn = dplyr::select(dplyr::arrange_at(dplyr::as_tibble(toreturn), c), -dummy)
  for(col in c) class(toreturn[[col]]) = class(x[[col]])
  
  return(toreturn)

}
