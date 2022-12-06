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
  loggit::set_logfile(file.path(output, "toc", "toc.log"))

  writeLines(
    text = capture.output(sessionInfo()),
    con = file.path(output, "toc", "RSessionInfo.extractor.txt")
  )


  message("\n########################################################\n")
  message("Extracting toc\n")

  toc_path <- file.path( input, "toc" )
  toc_files <- list.files(
    path = toc_path,
    pattern = "*.txt",
    full.names = TRUE,
    recursive = TRUE
  )
  toc_files <- grep("RSessionInfo", toc_files, value = TRUE, invert = TRUE)
  ##
  tmpdir <- file.path(output, "toc", "tmp")
  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
  ##
  lapply(
    toc_files,
    function(fn){
      message("Processing ", basename(fn)," ...")
      txt <- readLines(fn)


      # Split data file ---------------------------------------------------------


      message("  |- Splitting file ...")

      secEnd <- grep("\"Extra Samples\"", txt) - 1
      metadata <- txt[1:secEnd]
      metadata <- metadata[which(metadata != "" )]
      txt <- txt[-(1:secEnd)]

      secEnd <- grep("\"CurveNo\"", txt) - 1
      if (length(secEnd) == 0){
        secEnd <- grep("\"Inj. Type\"", txt) - 1
        calThere <- FALSE
      } else {
        calThere <- TRUE
      }
      parameter <- txt[1:secEnd]
      parameter <- parameter[which(parameter != "" )]
      txt <- txt[-(1:secEnd)]

      if (calThere){
        secEnd <- grep("\"Inj. Type\"", txt) - 1
        calib <- txt[1:secEnd]
        calib <- calib[which(calib != "" )]
        txt <- txt[-(1:secEnd)]
      }

      datatext <- txt
      datatext <- datatext[which(datatext != "" )]
      datatext <- gsub("\"", "", datatext)
      datatext <- gsub(" ", "", datatext)

      ### BEGIN TODO CHECK
      ### Replace "ZERO !"
      datatext <- gsub("ZERO!", 0, datatext)
      ### END TODO CHECK


      # Metadata --------------------------------------------------------------


      message("  |- Processing metadata ...")

      metadata <- utils::read.csv(
        text =  metadata,
        header = FALSE,
        col.names = c("name", "value"),
        stringsAsFactors = FALSE
      )
      metadata$name <- gsub(" |/|\\.\\.|\\.", "_", metadata$name)
      metadata$name <- tolower(metadata$name)


      # Parameter ----------------------------------------------


      message("  |- Processing parameter ...")

      parameter <- utils::read.csv(
        text = parameter,
        header = TRUE,
        stringsAsFactors = FALSE
      )
      colnames(parameter) <- gsub(" |/|\\.\\.|\\.", "_", colnames(parameter))
      colnames(parameter) <- tolower(colnames(parameter))


      # Calibration ----------------------------------------------

      if (calThere){
        message("  |- Processing calibration ...")

        calib <- utils::read.csv(
          text = calib,
          header = TRUE,
          stringsAsFactors = FALSE
        )
        colnames(parameter) <- gsub(" |/|\\.\\.|\\.", "_", colnames(parameter))
        colnames(parameter) <- tolower(colnames(parameter))
      }


      # Actual data --------------------------------------------------------


      message("  |- Processing actual data ...")

      no_samples <- max(parameter$samples) + max(parameter$extra_samples)
      no_cols <- 2 + 3 + no_samples * 2
      data_names <- c(
        "position", "identification",
        "inj_type", "conc", "cv",
        paste0(c("sample", "conc"), "_", rep(1:no_samples, each = 2))
      )

      sec_data <- utils::read.csv(
        text = datatext,
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

      if (gregexpr("S", data$identification) |> unlist() |> max() > 1){
        ## LEEF-1 format
        bottles <- sapply(
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

        timestamps <- sapply(
          strsplit(data$identification, "\\."),
          function(x){
            if (length(x) < 3){
              timestamp <- NA
            } else {
              day <- x[[1]]
              month <- x[[2]]
              year <- ifelse(
                as.integer(month) >= 9,
                2021,
                2022
              )
              timestamp <- paste0(year, month, day)
            }
            return(timestamp)
          }
        )
      } else if (gregexpr("S", data$identification) |> unlist() |> max() == 1){
        3# LEEF-2 format
        bottles <- substring(data$identification, 1, 3)
        bottles <- gsub("^S", "b_", bottles)

        timestamps <- substring(data$identification, 4, 11)
        timestamps <- gsub("^S", "b_", timestamps)
      }

      ad <- metadata[metadata$name=="date","value"]
      at <- metadata[metadata$name=="time","value"]

      anTime <- paste(as.Date(paste0(ad), "%A, %B %d, %Y"), at)

      data <- cbind(
        filename = fn,
        anaysis_time = anTime,
        timestamp = timestamps,
        bottle = bottles,
        data
      )




      # Saving ------------------------------------------------------------------


      message("  |- Saving files ...")
      utils::write.csv( metadata,  file.path(tmpdir, paste0(fn, ".metadata.csv"   )), row.names = FALSE)
      utils::write.csv( parameter, file.path(tmpdir, paste0(fn, ".parameter.csv"  )), row.names = FALSE)
      if (calThere){
        utils::write.csv( calib,     file.path(tmpdir, paste0(fn, ".calibration.csv")), row.names = FALSE)
      }
      utils::write.csv( data,      file.path(tmpdir, paste0(fn, ".data.csv"       )), row.names = FALSE)
    }
  )
  ##

  ftc <- list.files(file.path(input, "toc", "."), full.names = TRUE)
  ftc <- grep("\\.txt$", ftc, value = TRUE, invert = TRUE)
  pdfs <- grep("\\.pdf$", ftc, value = TRUE)
  ftc <- grep("\\.pdf$", ftc, value = TRUE, invert = TRUE)

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
