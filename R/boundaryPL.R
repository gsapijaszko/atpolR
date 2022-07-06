#' boundaryPL reads the file data/pl_boundary.Rds with simplified boundary geometry.
#' @return Simple Feature (sf) geometry of Poland in EPSG:2180 projection.
#' @export
boundaryPL <- function() {
  boundary <- readRDS(system.file("extdata", "pl_boundary.Rds", package = "atpolR"))
  return(boundary)
}
