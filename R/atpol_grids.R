utils::globalVariables(c("Name", ".data"))
#
#' .atpol10k_full reads ATPOL 10km x 10km grid from file and returns it as sf object
#' @importFrom terra vect
#' @importFrom sf st_as_sf
#' @return Simple Feature (sf) grid of polygons for 10kmx10km ATPOL grid
#' @usage .atpol10k_full()
.atpol10k_full <- function() {
  if(file.exists(system.file("extdata", "atpol10k.Rds", package = "atpolR"))) {
    v <- terra::vect(readRDS(system.file("extdata", "atpol10k.Rds", package = "atpolR", mustWork = TRUE)))
    v <- sf::st_as_sf(v)
    return(v)
  }
  else {
    print("No extdata/atpol10k.Rds data found.")
  }
}

#' atpol10k returns ATPOL grid 10x10 km and returns it as sf object
#' @importFrom sf st_centroid
#' @return Simple Feature (sf) grid of polygons for 10kmx10km ATPOL grid
#' @export
#' @usage atpol10k()
#'
atpol10k <- function() {
  grid10k <- subset(.atpol10k_full(), !grepl("AA|AF|AG|BG|CG", .atpol10k_full()$Name))
  grid10k$centroid <- sf::st_centroid(grid10k$geometry)
  return(grid10k)
}

#' atpol100k returns ATPOL grid 100km x 100km and returns it as sf object
#' @importFrom stats aggregate setNames
#' @return Simple Feature (sf) grid of polygons for 100kmx100km ATPOL grid
#' @export
#' @usage atpol100k()
#'
atpol100k <- function() {
  grid100k <- setNames(aggregate(atpol10k()[,"geometry"], by = list(substr(atpol10k()$Name, 1, 2)), mean), c("Name", "geometry"))
  return(grid100k)
}
