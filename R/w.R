#' Write
#'
#' Improved write function. Writes to csv without row names and automatically adds .csv to the file name if it isn't there already. Changes to .csv if another extension is passed.
#' Easier to type than write.csv(row.names=F).
#' Author: Bryce Chamberlain. Tech reveiw: Maria Gonzalez.
#'
#' @param x Data frame to write to file.
#' @param filename (Optional) Filename to use.
#' @param row.names (Optional) Specify if you want to include row names/numbers in the output file.
#' @param na (Optional) String to print for NAs. Defaults to an empty/blank string.
#'
#' @export
#'
#' @examples
#' # write the cars dataset.
#' path = paste0(tempdir(), '/out.csv')
#' w(cars, path)
#' 
#' # cleanup.
#' file.remove(path)
w <- function(x, filename = 'out', row.names = FALSE, na = ''){
  
  # force csv extension.
  filename <- paste0(gsub( '[.][a-z]+$','', filename, ignore.case = TRUE), '.csv' ) 
  
  # use faster fwrite. 
  data.table::fwrite(x = x, file = filename, row.names = row.names, na = na)
  
}