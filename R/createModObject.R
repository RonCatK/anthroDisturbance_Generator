#' This function checks a sim list for a specific object at a specific time, or
#' searches for it in an input folder (i.e. saved outputs). This function
#' simulates the existence of a sim list with specific objects in time t.
#'
#' @param data character string of the object or file name to be searched for
#'
#' @param fun function to be used to recover the file. Default is `readRDS`
#'
#' @param sim simList. Default is `NULL`.
#'
#' @param pathInput path to the directory containing the files to be searched for.
#'
#' @param currentTime Numeric. Current time to be used to serach for the object.
#'
#' @return A list of the objects that were read from disk. Currently the
#'             function only works with `.tif` (i.e. raster) and `.rds` objects.
#'
#' @author Tati Micheletti
#' @export
#' @include grepMulti.R
#' @importFrom crayon green magenta red
#' @importFrom raster raster
#' @importFrom SpaDES.core paddedFloatToChar
#' @rdname createModObject

createModObject <- function(data, 
                            sim = NULL, 
                            pathInput, 
                            currentTime, 
                            fun = readRDS,
                            returnNULL = FALSE){
  if (all(is.null(sim), is.null(pathInput)))
    stop("Either a simList or a folder containing the data need to be supplied")
  dt <- NULL
  if (!is.null(sim) & !is.null(sim[[data]])){
    dt <- sim[[data]]
  } else {
    message(crayon::yellow(paste0(data, " not supplied by another module.",
                                  " Will try using files in inputPath(sim)")))
    if (length(list.files(pathInput,
                          recursive = TRUE)) == 0) {
      if (returnNULL){
        message(crayon::red(paste0("The file for ", data, " was not found. Returning NULL")))
        return(NULL)
      } else {
        stop(paste0("Please place the data in the input folder ", pathInput))
      }
  }
    if (class(currentTime) != "numeric")
      stop("Current time needs to be numeric!")
    dataName <- grepMulti(x = list.files(pathInput,
                                         recursive = TRUE),
                          patterns = c(data, SpaDES.core::paddedFloatToChar(currentTime, padL = 3)))
    if (length(dataName) == 0){
      dt <- NULL
    } else {
      dt <- tryCatch({
        do.call(what = fun, args = list(file.path(pathInput, dataName)))
        }, error = function(e){
        NULL
      })
    }
    if (!is.null(dt)){
      message(paste0(data, " loaded from " ,
                     crayon::magenta(file.path(pathInput, dataName)),
                     " for year ", SpaDES.core::paddedFloatToChar(currentTime, padL = 3)))
    } else message(crayon::red(paste0("No file found for ", currentTime,". Returning NULL")))
  }
  return(dt)
}