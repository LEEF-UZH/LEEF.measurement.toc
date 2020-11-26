#' Check if data in input folder is OK and move to raw data folder
#'
#' @param input The folder, where a folder \code{toc} is located which
#'   contains the new files.
#' @param output A folder, which contains a subfolder called \code{toc}, i.e.
#'   the usually the raw data folder, into which the files will be moved to.
#'
#' @return a \code{list} which contains the individual results for each file.
#'   \code{TRUE} if moved, \code{FALSE} if an error occurred. Details of the error
#'   are in the error files in the \code{input/toc} directory.
#' @importFrom parallel mclapply
#' @importFrom utils capture.output
#' @export
#'
add_new_data <- function(input, output) {
  ##
  dir.create(
    file.path(output, "toc"),
    showWarnings = FALSE,
    recursive = TRUE
  )


  # Check and move folder ------------------------------------------------------

  files <- list.files(
    path = input,
    pattern = "\\.txt",
    full.names = FALSE
  )

  ##
  ok <- parallel::mclapply(
    files,
    function(f) {
      processing <- file.path(input, paste0("CHECKING.", f, ".CHECKING"))
      error <- file.path(input, paste0("ERROR.", f, ".txt"))

      on.exit(
        {
          if (file.exists(processing)) {
            unlink(processing)
            utils::capture.output(print(result), file = error)
          }
        }
      )
      ##
      file.create( processing )
      ##
      message("checking ", f)
      result <- list(
        ok = TRUE
      )

      # Check if file exist ----------------------------------------------------------

      ok <- list()

      ok$lines <- length(readLines(file.path(input, f))) >= 15

      result$ok <- all(unlist(result))

      if ( result$ok ) {
        file.copy(
          from = file.path(input, f),
          to = file.path(output, "toc"),
          recursive = FALSE,
          overwrite = TRUE
        )
        unlink( file.path(input, f) )
        unlink(processing)
      }
      return(result)
    }
  )
  names(ok) <- files
  return(ok)
}

