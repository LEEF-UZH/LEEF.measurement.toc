#' Preprocessor respirometer toc data
#'
#' Split files in parts and save as csv
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom utils read.csv write.csv
#'
#' @export

pre_processor_toc <- function(
  input,
  output
) {
  cat("\n########################################################\n")
  cat("\nProcessing toc\n")
  ##
  toc_path <- file.path( input, "toc" )
  toc_files <- list.files(
    path = toc_path,
    pattern = "*.txt",
    full.names = TRUE,
    recursive = TRUE
  )
  ##
  tmpdir <- tempfile()
  dir.create(tmpdir)
  ##
  lapply(
    toc_files,
    function(fn){
      breaks <- which(readLines(fn) == "")
      ##
      header <- utils::read.csv(fn, skip = 0, nrows = breaks[[1]], header = FALSE, row.names = 1, stringsAsFactors = FALSE)
      #
      layout <- utils::read.csv(fn, skip = breaks[[1]], nrows = breaks[[2]] - breaks[[1]] - 1, header = TRUE, stringsAsFactors = FALSE)
      #
      sampletime <- utils::read.csv(fn, skip = breaks[[2]], nrows = breaks[[3]] - breaks[[2]] - 1, header = FALSE, row.names = 1, stringsAsFactors = FALSE)
      #
      data1 <- utils::read.csv(fn, skip = breaks[[3]], nrows = breaks[[4]] - breaks[[3]] - 1, header = FALSE, fill = TRUE, stringsAsFactors = FALSE)
      data1 <- data1[,-ncol(data1)]
      names(data1) <- data1[1,]
      data1 <- data1[-1,]
      ##
      data2 <- utils::read.csv(fn, skip = breaks[[4]], header = FALSE, fill = TRUE, stringsAsFactors = FALSE)
      data2 <- data2[,-ncol(data2)]
      names(data2) <- data2[1,]
      data2 <- data2[-1,]
      ##
      target <- file.path( tmpdir, basename(fn))
      dir.create(
        target,
        recursive = TRUE,
        showWarnings = FALSE
      )
      ##
      utils::write.csv( header,     file.path(target, "header.csv"    ))
      utils::write.csv( layout,     file.path(target, "layout.csv"    ))
      utils::write.csv( sampletime, file.path(target, "sampletime.csv"))
      utils::write.csv( data1,      file.path(target, "data1.csv"     ))
      utils::write.csv( data1,      file.path(target, "data2.csv"     ))
    }
  )
  ##
  dir.create(
    file.path( output, "toc"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  file.copy(
    file.path( tmpdir, "." ),
    to = file.path( output, "toc" ),
    recursive = TRUE
  )
  ##
  unlink(tmpdir)
  ##
  cat("done\n")
  cat("\n########################################################\n")

  invisible(TRUE)
}
