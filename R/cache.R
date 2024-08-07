#' Initialize cache.
#'
#' Set cache info so easyr can manage the cache.
#'
#'
#' @param caches List of lists with properties name, depends.on. See example.
#' @param at.path Where to save the cache. If NULL, a cache/ folder will be created in the current working directory.
#' @param verbose Print via cat() information about cache operations.
#' @param save.only Choose not to load the cache. Use this if you need to check cache validity in multiple spots but only want to load at the last check.
#' @param skip.missing Passed to hashfiles, choose if an error occurs if a depends.on file isn't found.
#' @param n_processes Passed to qs to determine how many cores/workers to use when reading/saving data.
#'
#' @export
#'
#' @examples
#' # initialize a cache with 1 cache which depends on files in the current working directory.
#' # this will create a cache folder in your current working directory.
#' # then, you call functions to check and build the cache.
#' \dontrun{
#'
#'   folder = system.file('extdata', package = 'easyr')
#'   cache.init(
#'
#'    # Initial file read (raw except for renaming).
#'    caches = list(
#'      list( 
#'       name = 'prep-files',
#'       depends.on = paste0(folder, '/script.R')
#'      )
#'    ),
#' 
#'    at.path = paste0(tempdir(), '/cache')
#'
#'   )
#'
#' }
cache.init = function(caches, at.path, verbose = TRUE, save.only = FALSE, skip.missing = TRUE, n_processes = 2){
  
  validatecaches(caches)

  # unlock bindings so we can modify easyr resources related to cache.
  env = rlang::current_env()
  for( i in c( 'easyr.cache.info', 'cache.at', 'max.cache.loaded','cache.path' ) ){
    base::unlockBinding( i, env = parent.env( env = env ) )
    rm(i)
  }

    easyr.cache.info <<- list(
        cache.num = 0,
        cache.invalidated = FALSE,
        max.cache = 0,
        max.cache.loaded = 0,
        verbose = verbose
    )

  cache.info = caches
  easyr.cache.info$cache.info <<- cache.info

  easyr.cache.info$verbose <<- verbose
  easyr.cache.info$save.only <<- save.only
  easyr.cache.info$n_processes <<- n_processes
    
    # We need a directory if it doesn't already exist.
    if( !dir.exists(at.path) ) dir.create(at.path)

    # Add calculated paths.
    for( i in 1:length(cache.info) ){
        cache.info[[i]]$cache.num = i
        cache.info[[i]]$path = cc(at.path, '/', i, '-', cache.info[[i]]$name, '.qs')
        cache.info[[i]]$status.path = cc(at.path, '/', i, '-', cache.info[[i]]$name, '-status.RDS')
        rm(i)
    }

    # Loop through available cache.info, check the hash and delete any invalidated cache.info.
    easyr.cache.info$max.cache <<- 0
    if( easyr.cache.info$verbose ) cat( 'checking validity of cache \n' )
    
    for( i in 1:length(cache.info) ){

        if(file.exists(cache.info[[i]]$status.path)) cache_status = readRDS(cache.info[[i]]$status.path)
        
        # once the cache is invalidated or outdated, all downstream cache.info are invalid.
        if( 
          easyr.cache.info$cache.invalidated || 
          !file.exists(cache.info[[i]]$path) || 
          (exists('cache_status') && methods::is(cache_status, 'character')) # old cache_status will be a string. 
        ){ 
        
          clear.cache( cache.info[[i]] )
        
        # if there is no status, delete the cache file.
        } else if( !file.exists( cache.info[[i]]$status.path ) ){
        
          if( file.exists( cache.info[[i]]$path ) ) clear.cache( cache.info[[i]] )
        
        # Otherwise check the status.
        } else if( file.exists( cache.info[[i]]$path ) ){ 

          status.valid = cache_status$dependson_hash == hashfiles( cache.info[[i]]$depends.on, skip.missing = skip.missing )
        
        if( status.valid ){
            easyr.cache.info$max.cache <<- i
        } else {
            clear.cache( cache.info[[i]] )
        }
        
        rm( status.valid )
        
        }
        
    }
  
  easyr.cache.info$cache.info <<- cache.info
  
}


#' Check Cache Status
#' 
#' Check a cache and if necessary clear it to trigger a re-cache.
#'
#' @param cache.num The index/number for the cache we are checking in the cache.info list.
#' @param do.load Load the cache if it is found.
#'
#' @return Boolean indicating if the cache is acceptable. FALSE indicates the cache doesn't exist or is invalid so code should be run again.
#' @export
#'
#' @examples
#' # check the first cache to see if it exists and dependent files haven't changed.
#' # if this is TRUE, code in brackets will get skipped and the cache will be loaded instead.
#' # set do.load = FALSE if you have multiple files that build a cache, 
#' #    to prevent multiple cache loads.
#' # output will be printed to the console to tell you if the cache was loaded or re-built.
#' \dontrun{
#'   if( ! cache.ok(1) ){
#' 
#'     # do stuff
#'   
#'     # if this is the final file for this cache, 
#'     #   end with save.cache to save passed objects as a cache.
#'     save.cache(iris)
#'   }
#' }
cache.ok = function( cache.num, do.load = TRUE ){

    if( length(easyr.cache.info$cache.info) == 0 ) stop( 'easyr::cache.ok Error: Cache not set up correctly. Error E356-1 cache.' )

    # save current cache number to global so that save.cache runs correctly.
    checked.already = easyr.cache.info$cache.num == cache.num
    easyr.cache.info$cache.num <<- cache.num

    # check against the maximum valid cache. if it is less, don't load anything just return true.    if(cache.num==2) browser()
    if( easyr.cache.info$max.cache > easyr.cache.info$cache.num && do.load && ! easyr.cache.info$save.only ){
        if( !checked.already && easyr.cache.info$verbose ) cat( '\t   >> skip cache [', easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path, ']. \n' )
        cache.show_warnings(easyr.cache.info$cache.num)
        return( TRUE )

    # if it is the highest available cache, load it.
    } else if( easyr.cache.info$max.cache == easyr.cache.info$cache.num && do.load && ! easyr.cache.info$save.only ){
        if( !checked.already ){
            if( easyr.cache.info$verbose ) cat( '\t   >> load cache [', easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path, ']. \n' )
            load.cache(easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path)
            easyr.cache.info$max.cache.loaded <<- max.cache.loaded
            cache.show_warnings(easyr.cache.info$cache.num)
        }
        return( TRUE )

    } else {
        if( !checked.already ){
            if( 
              !easyr.cache.info$save.only &&  
              file.exists(easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path) &&
              easyr.cache.info$max.cache > 0 && 
              ( is.null(easyr.cache.info$max.cache.loaded) || easyr.cache.info$max.cache.loaded < easyr.cache.info$max.cache )              
            ){
                if( easyr.cache.info$verbose ) cat( '\t   >> load cache [', easyr.cache.info$cache.info[[ easyr.cache.info$max.cache ]]$path, ']. \n' )
                load.cache(easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path)
            }
            if( easyr.cache.info$verbose ) cat( '\t   >> build cache [', easyr.cache.info$cache.info[[ easyr.cache.info$cache.num ]]$path, ']. \n' )
        }
        return( FALSE )
    }

    # If you made it this far, load the cache and return true.
    
    return( TRUE )

}

#' Save Cache
#'  
#' Saves the arguments to a cache file, using the cache.num last checked with cache.ok.
#'
#' @param ... Objects to save.
#'
#' @export
#'
#' @examples
#' # check the first cache to see if it exists and dependent files haven't changed.
#' # if this check is TRUE, code in brackets will get skipped and the cache will be loaded instead.
#' # set do.load = FALSE if you have multiple files that build a cache, 
#' #    to prevent multiple cache loads.
#' # output will be printed to the console to tell you if the cache was loaded or re-built.
#' \dontrun{
#'   if( ! cache.ok(1) ){
#' 
#'     # do stuff
#'   
#'     # if this is the final file for this cache, 
#'     #   end with save.cache to save passed objects as a cache.
#'     save.cache(iris)
#' 
#'   }
#' 
#' }
save.cache = function( ... ){

    if('qs' %in% utils::installed.packages()){

      if( length(easyr.cache.info$cache.info) == 0 ) stop( 'easyr::cache.ok Error: Cache not set up correctly. Error E356-2 cache.' )
      
      # save status/hash and warnings..
      saveRDS(
          list(
            dependson_hash = hashfiles( easyr.cache.info$cache.info[[easyr.cache.info$cache.num]]$depends.on, skip.missing = TRUE),
            captured_warnings = easyr.cache.info$captured.warnings
          ),
          file = easyr.cache.info$cache.info[[easyr.cache.info$cache.num]]$status.path
      )
      easyr.cache.info$captured.warnings <<- NULL # reset for the next cache.

      cache.at <<- lubridate::now()
      cache.path <<- easyr.cache.info$cache.info[[easyr.cache.info$cache.num]]$path
      
      easyr.cache.info$max.cache.loaded <<- easyr.cache.info$cache.num
      if( easyr.cache.info$max.cache.loaded  > easyr.cache.info$max.cache ) easyr.cache.info$max.cache <<- easyr.cache.info$cache.num
      
      max.cache.loaded <<- easyr.cache.info$max.cache.loaded
      
      # save data. 
      datalist = list(...)
      names(datalist) = trimws(strsplit(gsub('save.cache\\(([^)]+)\\)', '\\1', cc(deparse(sys.call()))), ",")[[1]])
      datalist$cache.at = cache.at
      datalist$max.cache.loaded = max.cache.loaded
      datalist$cache.path = cache.path
      qs::qsave(datalist, file = cache.path, nthreads = easyr.cache.info$n_processes)

    } else {
      warning('Package [qs] not installed. Cache not saved.')
    }

}

#' Save Cache (Alternate)
#'  
#' Saves the arguments to a cache file, using the cache.num last checked with cache.ok.
#' This function provides an alternative syntax more aligned with other functions that start with "cache.".
#'
#' @param ... Objects to save.
#'
#' @export
#'
#' @examples
#' # check the first cache to see if it exists and dependent files haven't changed.
#' # if this check is TRUE, code in brackets will get skipped and the cache will be loaded instead.
#' # set do.load = FALSE if you have multiple files that build a cache, 
#' #    to prevent multiple cache loads.
#' # output will be printed to the console to tell you if the cache was loaded or re-built.
#' \dontrun{
#'   if( ! cache.ok(1) ){
#' 
#'     # do stuff
#'   
#'     # if this is the final file for this cache, 
#'     #   end with cache.save to save passed objects as a cache.
#'     cache.save(iris)
#' 
#'   }
#' 
#' }
cache.save = save.cache

#' Capture Warning
#' 
#' Utility function for capturing warnings.
#'
#' @param w Captured warning passed by withCallingHandlers.
#'
#' @export
#'
#' @examples
#' # this will only have an effect if a current cache exists.
#' \dontrun{
#'   if(!cache.ok(1)) withCallingHandlers({
#'      x = mtcars # base-R dataset.
#'      x = mtcars # base-R dataset.
#'        warning('warning 2-1') # this is the first warning we need tdo capture. 
#'        warning('warning 2-2') # this is the first warning we need tdo capture. 
#'        save.cache(x) # we'll capture it inside svae.caceh
#'   }, warning = cache.capture_warning)
#' }
#' 
cache.capture_warning = function(w){
  easyr.cache.info$captured.warnings <<- c(easyr.cache.info$captured.warnings, w$message)
}

#' Clear Cache
#' 
#' Clears all caches or the cache related to the passed cache info list.
#'
#' @param cache The cache list to clear.
#'
#' @return FALSE if a cache info list item is passed in order to assist other functions in returning this value, otherwise NULL. 
#' @export
#'
#' @examples
#' # this will only have an effect if a current cache exists.
#' \dontrun{
#'   clear.cache()
#' }
#' 
clear.cache = function( cache = NULL ){

    if( length(easyr.cache.info$cache.info) == 0 ) stop( 'easyr::cache.ok Error: Cache not set up correctly. Error E356-3 cache.' )

    # determine which cache to clear
    # if nothing passed, clear them all
    if( is.null( cache ) ){
        do.caches = easyr.cache.info$cache.info
        
    # if numeric, assume an index was passed and select the relevant caches.
    } else if( is.numeric(cache) ){
        do.caches = list( easyr.cache.info$cache.info[[ cache ]] )
        
    # otherwise assume a cache list was passed.
    } else {
      do.caches = list(cache)
    }

    for( icache in do.caches ){

        if( file.exists( icache$status.path ) ) file.remove( icache$status.path )
        if( file.exists( icache$path ) ) file.remove( icache$path )
      
        if( easyr.cache.info$max.cache.loaded >= icache$cache.num ) easyr.cache.info$max.cache.loaded <<- icache$cache.num - 1
        if( easyr.cache.info$max.cache >= icache$cache.num ) easyr.cache.info$max.cache <<- icache$cache.num - 1

    }

    easyr.cache.info$cache.invalidated <<- TRUE
    if( !is.null( cache ) ) return(FALSE)

}

# utils
validatecaches = function( caches ){
  
  if( ! is.list(caches) ) stop( '[caches] must be a list of lists.' )
  
  for( i in caches ){
    
    if( ! is.list(i) ) stop( '[caches] must be a list of lists.' )
    
    missing.properties = setdiff( c( 'name', 'depends.on' ), names(i) ) 
    if( length(missing.properties) > 0 ) stop( 'Cache is missing property(s): [', cc( missing.properties, sep = ', ' ), '].' )
    
    rm(i,missing.properties)
    
  }
  
}

load.cache = function(filename) if('qs' %in% utils::installed.packages()){
  list2env(qs::qread(filename, nthreads = easyr.cache.info$n_processes), globalenv())
} else {
    warning('Package [qs] not installed. Cache not loaded.')
}

easyr.cache.info = list(
    cache.num = 0,
    cache.invalidated = FALSE,
    max.cache = 0,
    max.cache.loaded = 0,
    verbose = TRUE,
    captured.warnings = NULL
)
cache.at = NULL
max.cache.loaded = -1
cache.path = 'cache'

cache.show_warnings = function(cache.num){
  showwarnings = readRDS(easyr.cache.info$cache.info[[easyr.cache.info$cache.num]]$status.path)$captured_warnings
  if(length(showwarnings) > 0) warning(glue::glue(
    'from cache {cache.num}: ({easyr.cache.info$cache.info[[cache.num]]$name}):\n\t{cc(showwarnings, sep = "\n\t")}'
  ))
}
