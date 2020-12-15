#' Extractor toc data
#'
#' Convert all \code{.cvs} files in \code{toc} folder to \code{data.frame} and save as \code{.rds} file.
#'
#' This function is extracting data to be added to the database (and therefore make accessible for further analysis and forecasting)
#' from \code{.csv} files.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom yaml read_yaml
#' @importFrom utils write.csv
#' @export
#'
extractor_toc <- function(
  input,
  output
) {
  message("\n########################################################\n")
  message("Extracting toc\n")

  # Get csv file names ------------------------------------------------------

  toc_path <- file.path( input, "toc" )
  toc_files <- list.files(
    path = toc_path,
    pattern = "*.csv",
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(toc_files) == 0) {
    message("nothing to extract\n")
    message("\n########################################################\n")
    return(invisible(FALSE))
  }

# Extract ---------------------------------------------------------------

  add_path <- file.path( output, "toc" )

# Read file and save as rds -------------------------------------------------

  for (fn in toc_files) {
    fnout <- gsub( normalizePath(input), normalizePath(output), normalizePath(fn) )
    
    dir.create( dirname(fnout), recursive = TRUE, showWarnings = FALSE  )
    dat <- read.csv( fn )
    ##
    timestamp <- yaml::read_yaml(file.path(input, "toc", "sample_metadata.yml"))$timestamp
    dat <- cbind(timestamp = timestamp, dat)
    ##
    utils::write.csv( 
    	dat, 
    	file = fnout,
    	row.names = FALSE
	)
  }

  file.copy(
    from = file.path(input, "toc", "sample_metadata.yml"),
    to = file.path(output, "toc", "sample_metadata.yml")
  )

# Finalize ----------------------------------------------------------------

  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
