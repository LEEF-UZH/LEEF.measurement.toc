#' Register the processing of toc data in the LEEF.Data package
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @export
#'
register <- function() {
  if (is.null(system.file(package = "LEEF.Data"))) {
    stop("This function requres the package to be installed!")
  }

  LEEF.Data::add_pre_processor( pre_processor_toc )
  LEEF.Data::add_extractor( extractor_toc )
  ##
  invisible(TRUE)
}

