#' Hash Files
#' 
#' Get a hash value representing a list of files. Useful for determining if files have changed in order to reset dependent caches.
#'
#' @param x Input which specifies which files to hash. This can be a vector mix of paths and files.
#' @param skip.missing Skip missing files. Default is to throw an error if a file isn't found.
#' @param full.hash By default we just hash the file info (name, size, created/modified time). Set this to TRUE to read the file and hash the contents.
#' @param verbose Print helpful messages from code.
#' @param skiptemp Skip temporary MS Office files like "~$Simd Loss Eval 2018-06-30.xlsx"
#'
#' @return String representing hash of files.
#' @export
#'
#' @examples
#' folder = system.file('extdata', package = 'easyr')
#' hashfiles(folder)
hashfiles = function(x, skip.missing = FALSE, full.hash = FALSE, verbose = FALSE, skiptemp = TRUE){
  
  hash.out = NULL
  
  for(i in x){
    
    if(dir.exists(i)){
      ifiles = list.files(i, full.names = TRUE, recursive = TRUE)
    } else{
      ifiles = i
    }
    
    ifiles = ifiles[!grepl('(git|node_modules|venv)/', ifiles)]
    if(skiptemp) ifiles = ifiles[!grepl('~[$]', ifiles)]
    
    if(verbose) print(glue::glue('easyr::hashfiles: hashing [{fmat(length(ifiles), ",")}] files from [{i}].'))

    # parallel is slower for only a few files. 
    if(length(ifiles) > 50){
      cl = parallel::makeCluster(parallel::detectCores() - 1)
      parallel::clusterEvalQ(cl, require(digest))
      file_hashes = unlist(parallel::parLapply(cl, ifiles, hashfile, skip.missing = skip.missing, full.hash = full.hash))
      parallel::stopCluster(cl)
      rm(cl)
    } else {
      file_hashes = unlist(lapply(ifiles, hashfile, skip.missing = skip.missing, full.hash = full.hash))
    }
    
    # add to the running hash.
    hash.out = c(sort(file_hashes), hash.out) # parallel might re-sort things.
    
  }
  
  hash.out = digest::digest(hash.out, algo = "crc32", serialize = FALSE)
  
  return( hash.out )
  
}

hashfile = function(path, skip.missing, full.hash){
  
  if(!file.exists(path)){
    if(!skip.missing){
      stop(glue::glue('easyr::hashfiles File not found: [{path}]. Error E954 hashfiles'))
    } else {
      return(NULL)
    }
  }

  info = file.info(path)
  if(full.hash){
    data = readBin(path, what = "raw", n = info$size)
    result = digest::digest(data, algo = "crc32")
    # if(is.null(result)) stop( glue::glue( "Error at digest::digest for [{path}] Error E1140 hashfiles"))
    return(result)
  } else {
    return(digest::digest(c(info$mtime, info$size), algo = "crc32"))
  }

}

