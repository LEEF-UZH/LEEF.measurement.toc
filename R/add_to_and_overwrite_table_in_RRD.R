#' Add files to database
#'
#' @param toc the (\code{data.frame}) containing the toc data for the database
#' @param dbname name and path to the database in which the data should be written
#'
#' @return
#'
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @export
#'
#' @examples
add_to_and_overwrite_table_in_RRD <- function(
    toc,
    dbname
){

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
    value = toc,
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
