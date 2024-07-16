#' Write
#'
#' Improved write function. Writes to csv or excel without row names and automatically adds .csv to the file name if it isn't there already. Changes to .csv if a non-xlsx extension is passed.
#' Easier to type than write.csv(row.names=F) or write.excel().
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

  ext = stringr::str_extract(filename, '[.][^.]+$')
  filename = gsub('[.][^.]+$', '', filename)

  if(is.na(ext) || ext %ni% c('.csv', '.xlsx')) ext = '.csv'
  if(ext == '.csv'){
    data.table::fwrite(x = x, file = paste0(filename, ext), row.names = row.names, na = na)
  } else if(ext == '.xlsx'){
    openxlsx::write.xlsx(x = x, file = paste0(filename, ext), rowNames = FALSE)
  }
  
}