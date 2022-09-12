#' Preprocessor toc data
#'
#' Copy original text files
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom utils read.csv write.csv
#' @importFrom loggit set_logfile
#'
#' @export

pre_processor_toc <- function(
    input,
    output
) {
  if ( length( list.files( file.path(input, "toc") ) ) == 0 ) {
    message("\nEmpty or missing toc directory - nothing to do.\n")
    message("\ndone\n")
    message("########################################################\n")
    return(invisible(TRUE))
  }

  dir.create(
    file.path(output, "toc"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  loggit::set_logfile(file.path(output, "toc", "toc.log"))

  message("\n########################################################\n")
  message("\nProcessing toc\n")
  ##



  file.copy(
    file.path( input, "..", "00.general.parameter", "." ),
    file.path( output, "toc" ),
    recursive = TRUE,
    overwrite = TRUE
  )

  file.copy(
    from = file.path(input, "toc", "."),
    to = file.path(output, "toc"),
    recursive = TRUE
  )


  ##
  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
