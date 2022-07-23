utils::globalVariables(c("Name", ".data"))
#
#' .atpol10k_full reads ATPOL 10km x 10km grid from file and returns it as sf object
#' @importFrom terra vect
#' @importFrom sf st_as_sf
#' @return Simple Feature (sf) grid of polygons for 10km x 10km ATPOL grid
#' @usage .atpol10k_full()
#' @noRd
#'
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
#' @return Simple Feature (sf) grid of polygons for 10km x 10km ATPOL grid
#' @export
#' @usage atpol10k()
#'
atpol10k <- function() {
  grid10k <- subset(.atpol10k_full(), !grepl("AA|AF|AG|BG|CG", .atpol10k_full()$Name))
  grid10k$centroid <- sf::st_centroid(grid10k$geometry)
  return(grid10k)
}

#' atpol100k creates ATPOL grid 100km x 100km and returns it as sf object
#' @importFrom stats aggregate setNames
#' @return Simple Feature (sf) grid of polygons for 100km x 100km ATPOL grid
#' @export
#' @usage atpol100k()
#'
atpol100k <- function() {
  grid100k <- setNames(aggregate(atpol10k()[,"geometry"], by = list(substr(atpol10k()$Name, 1, 2)), mean), c("Name", "geometry"))
  return(grid100k)
}

#' atpol1k creates ATPOL grid 1km x 1km and returns it as sf object
#' @importFrom terra vect project
#' @importFrom sf st_as_sf
#' @return Simple Feature (sf) grid of polygons for 1km x 1km ATPOL grid
#' @param grid any valid ATPOL 10km grid like "BE23" or "DC58"
#' @export
#' @examples
#' atpol1k("BE23")
#' atpol1k(grid = c("BE23", "DE45"))
#'
atpol1k <- function(grid) {
  g <- toupper({{grid}})
  if (all(g[] %in% atpol10k()$Name) == FALSE) {
    stop("Please make sure all grids are proper ATPOL 10km grid squares, like 'BE23'")
  }
  else {
    v <- terra::vect()
    for(gr in g) {
      for(x1 in c(0:9)) {
        print(paste0("Generating grid for ",gr,x1,"..."))
        for(y1 in c(0:9)) {
          g0 = structure(c(
            grid_to_latlon(paste0(gr,x1,y1), 0, 0)[2],grid_to_latlon(paste0(gr,x1,y1), 0, 1)[2],
            grid_to_latlon(paste0(gr,x1,y1), 1, 1)[2],grid_to_latlon(paste0(gr,x1,y1), 1, 0)[2],
            grid_to_latlon(paste0(gr,x1,y1), 0, 0)[1],grid_to_latlon(paste0(gr,x1,y1), 0, 1)[1],
            grid_to_latlon(paste0(gr,x1,y1), 1, 1)[1],grid_to_latlon(paste0(gr,x1,y1), 1, 0)[1]),
            .Dim = c(4L, 2L))
          a <- terra::vect(g0, crs = "EPSG:4326", "polygons", atts = data.frame("Name" = paste0(gr,x1,y1))) |>
            terra::project("EPSG:2180")
          if (dim(v)[1] == 0) {
            v <- a
          } else { v <- v+a}
        }
      }
    }
    v <- terra::project(v, "EPSG:2180") |>
      sf::st_as_sf()
    return(v)
  }
}
