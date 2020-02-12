

.onLoad <- function(lib, pkg) {
  opt <-  list(
    debug = FALSE
  )
  options(LEEF.measurement.toc = opt)
}

# utils::globalVariables(c("FL1-H", "FL3-H", "FSC-A", "SSC-A", "Width"))
