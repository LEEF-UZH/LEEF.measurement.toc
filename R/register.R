#' Register the processing of toc data in the LEEF package
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @export
#'
register <- function() {
  if (is.null(system.file(package = "LEEF"))) {
    stop("This function requres the package LEEF to be installed!")
  }

  LEEF::add_pre_processor( pre_processor_toc )
  LEEF::add_extractor( extractor_toc )
  ##
  invisible(TRUE)
}

