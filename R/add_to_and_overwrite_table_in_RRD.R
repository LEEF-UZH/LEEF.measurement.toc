#' Add files to database
#'
#' @param input directory from which to read the data
#' @param dbname name and path to the database in which the data should be written
#' @param append if \code{TRUE}, data will be appended to the existing table.
#'   If \code{FALSE} (the default), an error will be raised if the table exists already,
#'   unless \code{overwrite = TRUE}
#' @param overwrite if \code{TRUE}, an existing table will be overwriten.
#'   If \code{FALSE} (the default), an error will be raised if the table exists already,
#'   unless \code{append = TRUE}
#'
#' @return
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @export
#'
#' @examples
add_to_and_overwrite_table_in_RRD <- function(
    input,
    dbname,
    append = FALSE,
    overwrite = FALSE
){
  data_files <- list.files(
    file.path(input, "toc"),
    pattern = "\\.data\\.csv$",
    full.names = TRUE
  )

  toc_data <- NULL
  for (fn in data_files){
    toc_data <- rbind(
      toc_data,
      read.csv(fn)
    )
  }

  conn <- NULL
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname )
  on.exit(
    try(
      DBI::dbDisconnect(conn),
      silent = TRUE
    )
  )

  DBI::dbWriteTable(
    conn,name = "toc__toc",
    value = toc_data,
    overwrite = TRUE
  )
  try(
    DBI::dbExecute(conn, "CREATE INDEX idx_toc__toc_timetamp on toc__toc(timestamp);")
  )
  try(
    DBI::dbExecute(conn, "CREATE INDEX idx_toc__toc_bottle on toc__toc(bottle);")
  )
  try(
    DBI::dbExecute(conn, "CREATE INDEX idx_toc__toc_timestamp_bottle on toc__toc(timestamp, bottle);")
  )

}
