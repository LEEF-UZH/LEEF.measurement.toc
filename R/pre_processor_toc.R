#' Preprocessor o2meter toc data
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
  message("\n########################################################\n")
  message("\nProcessing toc\n")
  ##
  toc_path <- file.path( input, "toc" )
  toc_files <- list.files(
    path = toc_path,
    pattern = "*.txt",
    full.names = TRUE,
    recursive = TRUE
  )
  ##
  tmpdir <- file.path(output, "tmp")
  dir.create(tmpdir)
  ##
  lapply(
    toc_files,
    function(fn){
      breaks <- which(readLines(fn) == "")
      ##
      header <- utils::read.csv(
        fn,
        skip = 0,
        nrows = breaks[[1]],
        header = FALSE,
        row.names = 1,
        col.names = c("name", "value"),
        stringsAsFactors = FALSE
      )
      rownames(header) <- gsub(" |/|\\.", "_", rownames(header))
      rownames(header) <- tolower(rownames(header))
      #
      layout <- utils::read.csv(
        fn,
        skip = breaks[[1]],
        nrows = breaks[[2]] - breaks[[1]] - 1,
        header = TRUE,
        stringsAsFactors = FALSE
      )
      colnames(layout) <- gsub(" |/|\\.\\.|\\.", "_", colnames(layout))
      colnames(layout) <- tolower(colnames(layout))
      #
      sampletime <- utils::read.csv(
        fn,
        skip = breaks[[2]],
        nrows = breaks[[3]] - breaks[[2]] - 1,
        header = FALSE,
        row.names = 1,
        col.names = c("name", "value"),
        stringsAsFactors = FALSE
      )
      rownames(sampletime) <- tolower(rownames(sampletime))
      #
      colNames <- c(
        "curve_no",
        "type",
        "filename",
        "date_time",
        "max_area",
        "volume",
        "max conc.",
        "correlation",
        "r_Squared",
        "sel",
        "range",
        "empty"
      )
      data1 <- utils::read.csv(
        fn,
        skip = breaks[[3]],
        nrows = breaks[[4]] - breaks[[3]] - 1,
        header = FALSE,
        fill = TRUE,
        stringsAsFactors = FALSE,
        col.names = colNames
      )
      data1 <- data1[-1,-ncol(data1)]
      ##
      data2 <- utils::read.csv(
        fn,
        skip = breaks[[4]],
        header = FALSE,
        fill = TRUE,
        stringsAsFactors = FALSE
      )
      data2 <- data2[,-ncol(data2)]
      names(data2) <- data2[1,]
      data2 <- data2[-1,]
      colnames(data2) <- gsub(" |/|\\.\\.|\\.|\\. ", "_", colnames(data2))
      colnames(data2) <- tolower(colnames(data2))
      ##
      target <- file.path( tmpdir, gsub("\\.txt$", "", basename(fn)) )
      dir.create(
        target,
        recursive = TRUE,
        showWarnings = FALSE
      )
      ##
      utils::write.csv( header,     file.path(target, "header.csv"),     row.names = FALSE)
      utils::write.csv( layout,     file.path(target, "layout.csv"),     row.names = FALSE)
      utils::write.csv( sampletime, file.path(target, "sampletime.csv"), row.names = FALSE)
      utils::write.csv( data1,      file.path(target, "data1.csv"),      row.names = FALSE)
      utils::write.csv( data1,      file.path(target, "data2.csv"),      row.names = FALSE)
    }
  )
  ##
  dir.create(
    file.path( output, "toc"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  file.copy(
  	file.path( input, "..", "00.general.parameter", "." ),
  	file.path( output, "toc" ),
  	recursive = TRUE,
  	overwrite = TRUE
  )
  file.copy(
    file.path( tmpdir, "." ),
    to = file.path( output, "toc" ),
    recursive = TRUE
  )
  ##
  unlink(tmpdir)
  file.copy(
    from = file.path(input, "sample_metadata.yml"),
    to = file.path(output, "toc", "sample_metadata.yml")
  )

  ##
  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
