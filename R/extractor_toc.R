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
#' @return invisibly the names of the csv files created
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom utils write.csv
#' @importFrom loggit set_logfile
#' @export
#'
extractor_toc <- function(
  input,
  output
) {
  message("Extracting toc\n")
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
  loggit::set_logfile(file.path(output, "o2meter", "o2meter.log"))

  message("\n########################################################\n")
  message("Extracting toc\n")

  toc_path <- file.path( input, "toc" )
  toc_files <- list.files(
    path = toc_path,
    pattern = "*.txt",
    full.names = TRUE,
    recursive = TRUE
  )
  ##
  tmpdir <- file.path(output, "toc", "tmp")
  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
  ##
  lapply(
    toc_files,
    function(fn){
      message("Processing ", fn," ...")
      txt <- readLines(fn)


      # Read Metadata -----------------------------------------------------------


      message("  |- Processing metadata ...")

      secEnd <- grep("\"Extra Samples\"", txt) - 1
      sec <- txt[1:secEnd]
      sec <- sec[which(sec != "" )]
      txt <- txt[-(1:secEnd)]

      header <- utils::read.csv(
        text =  sec,
        header = FALSE,
        col.names = c("name", "value"),
        stringsAsFactors = FALSE
      )
      header$name <- gsub(" |/|\\.\\.|\\.", "_", header$name)
      header$name <- tolower(header$name)

      rm(sec, secEnd)


      # Read Measurement Parameter ----------------------------------------------


      message("  |- Processing measurement parameter ...")

      secEnd <- grep("\"Inj. Type\"", txt) - 1
      sec <- txt[1:secEnd]
      txt <- txt[-(1:secEnd)]
      sec <- sec[which(sec != "" )]


      layout <- utils::read.csv(
        text = sec,
        header = TRUE,
        stringsAsFactors = FALSE
      )
      colnames(layout) <- gsub(" |/|\\.\\.|\\.", "_", colnames(layout))
      colnames(layout) <- tolower(colnames(layout))

      rm(sec, secEnd)

      # Read actual data --------------------------------------------------------


      message("  |- Processing actual data ...")

      sec <- txt
      rm(txt)
      sec <- sec[which(sec != "" )]

      sec <- gsub("\"", "", sec)
      sec <- gsub(" ", "", sec)

      ### BEGIN TODO CHECK
      ### Replace "ZERO !"
      sec <- gsub("ZERO!", -999, sec)
      ### END TODO CHECK

      no_samples <- max(layout$samples) + 1
      no_cols <- 2 + 3 + no_samples * 2
      data_names <- c(
        "position", "identification",
        "inj_type", "conc", "cv",
        paste0(c("sample", "conc"), "_", rep(1:no_samples, each = 2))
      )

      sec_data <- utils::read.csv(
        text = sec,
        header = TRUE,
        fill = TRUE,
        stringsAsFactors = FALSE,
        row.names = NULL
      )

      ## TC
      begin_col <- grep("Inj.Type", names(sec_data))[1]
      end_col <- grep("Inj.Type", names(sec_data))[2] - 1
      tc_data <- sec_data[,1:end_col]
      names(tc_data) <- data_names
      sec_data <- sec_data[,-(begin_col:end_col)]

      ## IC
      end_col <- grep("Inj.Type", names(sec_data))[2] -1
      ic_data <- sec_data[,1:end_col]
      names(ic_data) <- data_names
      sec_data <- sec_data[,-(begin_col:end_col)]

      ## TOC
      end_col <- grep("Inj.Type", names(sec_data))[2] -1
      toc_data <- sec_data[,1:end_col]
      nd <- ncol(toc_data)
      names(toc_data) <- data_names[1:nd]
      toc_data <- cbind(
        toc_data,
        ic_data[,(nd+1):ncol(ic_data)]
      )
      toc_data[,(nd+1):ncol(ic_data)] <- NA
      sec_data <- sec_data[,-(begin_col:end_col)]

      ## TN
      tn_data <- sec_data[-ncol(sec_data)]
      names(tn_data) <- data_names
      rm(sec_data)

      data <- rbind(
        tc_data,
        toc_data,
        ic_data,
        tn_data
      )

      data <- data[, -grep("sample_", names(data))]

      fn <- tools::file_path_sans_ext(basename(fn))

      bottle <- sapply(
        strsplit(data$identification, "\\."),
        function(x){
          if (length(x) < 3){
            b <- NA
          } else {
            b <- x[[3]]
            b <- gsub("^S", "b_", b)
          }
          return(b)
        }
      )

      timestamps <- gsub("LEEF_|A|B", "", fn)
      timestamps <- strsplit(timestamps, "und")[[1]]
      timestamps <- format(as.Date(timestamps, "%y_%m_%d"), "%Y%m%d")

      bn <- gsub("b_", "", bottle)
      bn <- as.integer(bn)
      ti <- 1
      timestamp <- rep(NA, length(bn))
      for (i in 1:(length(bn)-1)){
        if (is.na(bn[i])){
          timestamp[i] <- NA
          ti <- 1
        } else if (isTRUE( (bn[i] < bn[i+1]) | is.na(bn[i+1]) )) {
          timestamp[i] <- timestamps[ti]
        } else if (isTRUE(bn[i] > bn[i+1])) {
          timestamp[i] <- timestamps[ti]
          ti <- ifelse(
            ti == 1,
            2,
            1
          )
        }
      }
      i <- length(bn)
      if (is.na(bn[i])){
        timestamp[i] <- NA
      } else {
        timestamp[i] <- timestamps[ti]
      }


      ad <- header[header$name=="date","value"]
      at <- header[header$name=="time","value"]

      anTime <- paste(as.Date(paste0(ad), "%A, %B %d, %Y"), at)

      data <- cbind(
        filename = fn,
        anaysis_time = anTime,
        timestamp = timestamp,
        bottle = bottle,
        data
      )



      # Saving ------------------------------------------------------------------


      message("  |- Saving files ...")
      utils::write.csv( header, file.path(tmpdir, paste0(fn, ".header.csv")), row.names = FALSE)
      utils::write.csv( layout, file.path(tmpdir, paste0(fn, ".layout.csv")), row.names = FALSE)
      utils::write.csv( data,   file.path(tmpdir, paste0(fn, ".data.csv"  )), row.names = FALSE)

    }
  )
  ##

  ftc <- list.files(file.path(input, "toc", "."), full.names = TRUE)
  ftc <- grep("\\.txt$", ftc, value = TRUE, invert = TRUE)
  pdfs <- grep("\\.pdf$", ftc, value = TRUE)
  ftc <- grep("\\.pdf$", ftc, value = TRUE, invert = TRUE)

  file.copy(
    ftc,
    tmpdir,
    recursive = TRUE,
    overwrite = TRUE
  )
  dir.create(
    file.path( tmpdir, "pdf"),
    recursive = FALSE,
    showWarnings = FALSE
  )
  file.copy(
    pdfs,
    file.path( tmpdir, "pdf" ),
    recursive = TRUE,
    overwrite = TRUE
  )


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
  unlink(tmpdir, recursive = TRUE, force = TRUE)

# Finalize ----------------------------------------------------------------

  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
