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
#' @importFrom tools file_path_sans_ext
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
      message("Processing ", fn," ...")
      txt <- readLines(fn)
      breaks <- which(txt == "")


      # Read Metadata -----------------------------------------------------------


      message("  |- Processing metadata ...")
      header <- utils::read.csv(
        text =  txt[1:breaks[[1]]],
        header = FALSE,
        col.names = c("name", "value"),
        stringsAsFactors = FALSE
      )
      header$name <- gsub(" |/|\\.", "_", header$name)
      header$name <- tolower(header$name)


      # Read Measurement Parameter ----------------------------------------------


      message("  |- Processing measurement parameter ...")
      layout <- utils::read.csv(
        text = txt[(breaks[[1]]+1):breaks[[2]]],
        header = TRUE,
        stringsAsFactors = FALSE
      )
      colnames(layout) <- gsub(" |/|\\.\\.|\\.", "_", colnames(layout))
      colnames(layout) <- tolower(colnames(layout))
      #
      # sampletime <- utils::read.csv(
      #   fn,
      #   skip = breaks[[2]],
      #   nrows =  -1,
      #   header = FALSE,
      #   col.names = c("name", "value"),
      #   stringsAsFactors = FALSE
      # )
      # rownames(sampletime) <- tolower(rownames(sampletime))
      # #

      # Read actual data --------------------------------------------------------


      message("  |- Processing actual data ...")
      txt <- txt[-(1:breaks[[2]])]
      txt <- gsub("\"", "", txt)
      txt <- gsub(" ", "", txt)

      dat <- read.csv(text = txt)

      idcol <- strsplit(txt[1], ",")[[1]]
      idcol <- c(1, which (idcol == "Inj.Type"), length(idcol)+1)

      colNames <- c(
        "position",
        "identification",
        "tc_inj.type",
        "tc_conc.",
        "tc_cv",
        "tc_sample_1",
        "tc_concentration_1",
        "tc_sample_2",
        "tc_concentration_2",
        "tc_sample_3",
        "tc_concentration_3",
        "ic_inj.type",
        "ic_conc.",
        "ic_cv",
        "ic_sample_1",
        "ic_concentration_1",
        "ic_sample_2",
        "ic_concentration_2",
        "ic_sample_3",
        "ic_concentration_3",
        "toc_inj.type",
        "toc_conc.",
        "toc_cv",
        "tn_inj.type",
        "tn_conc.",
        "tn_cv",
        "tn_sample_1",
        "tn_concentration_1",
        "tn_sample_2",
        "tn_concentration_2",
        "tn_sample_3",
        "tn_concentration_3",
        "TBD"
      )


      colclasses <- c(
        "integer",
        "character",
        "NULL", # "character",
        "numeric",
        "numeric",
        "NULL", # "integer",
        "numeric",
        "NULL", # "integer",
        "numeric",
        "NULL", # "integer",
        "numeric",
        "NULL", # "character",
        "numeric",
        "numeric",
        "NULL", # "integer",
        "character",
        "NULL", # "integer",
        "character",
        "NULL", # "integer",
        "character",
        "NULL", # "character",
        "numeric",
        "logical",
        "NULL", # "character",
        "numeric",
        "numeric",
        "NULL", # "integer",
        "numeric",
        "NULL", # "integer",
        "numeric",
        "NULL", # "integer",
        "numeric",
        "logical"
      )

      data <- utils::read.csv(
        text = txt,
        header = TRUE,
        fill = TRUE,
        stringsAsFactors = FALSE,
        colClasses = colclasses,
        col.names = colNames,
        row.names = NULL
      )

      data <- data[,-ncol(data)]
      ##
      fn <- tools::file_path_sans_ext(basename(fn))
      fn <- gsub(" ", "", fn)
      fn <- gsub("-", "_", fn)


      # Saving ------------------------------------------------------------------

      message("  |- Saving files ...")
      utils::write.csv( header, file.path(tmpdir, paste0(fn, ".header.csv")), row.names = FALSE)
      utils::write.csv( layout, file.path(tmpdir, paste0(fn, ".layout.csv")), row.names = FALSE)
      utils::write.csv( data,   file.path(tmpdir, paste0(fn, ".data.csv"  )), row.names = FALSE)
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
