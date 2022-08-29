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
    warning("No extdata/atpol10k.Rds data found.")
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
        message(paste0("Generating grid for ",gr,x1,"..."))
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

#' atpol_div creates ATPOL grid divided by 2, 4 or 5 (based on divider parameter) and returns it as sf object. Useful for grids like 5 x 5 km (divider = 2), 250 x 250 m (divider = 4) or 20 x 20 m (divider = 5). For details see \insertCite{vereyStandaryzacjaZapisuPodzialow2018;textual}{atpolR}
#' @importFrom terra vect project
#' @importFrom sf st_as_sf
#' @return Simple Feature (sf) grid of polygons for ATPOL grid divided by 2, 4 or 5
#' @param grid any valid ATPOL grid like "BE" or "DC5128"
#' @param divider divide by parameter: 2, 4, 5
#' @export
#' @usage atpol_div(grid, divider)
#' @examples
#' \donttest{
#' atpol_div("BE", 2)
#' atpol_div(grid = c("BE23", "DC5128"), divider = 4)
#' }
#' @references
#'     \insertAllCited{}
#'
#'
atpol_div <- function(grid, divider) {
  g <- toupper({{grid}})
  if(!divider %in% c(2,4,5)) {
    stop("Please make sure divider is one of: 2, 4, 5")
  }
  if (all(substr(g, 1, 2)[] %in% unique(substr(atpol10k()$Name, 1,2))) == FALSE) {
    stop("Please make sure all grids are proper ATPOL grid squares, like 'BE' or 'CE2345'")
  } else if (all(nchar(g[]) %% 2 == 0) == FALSE) {
    stop("Please make sure length of all grid fields is 2, 4, 6, 8, 10 or 12")
  } else {
    v <- terra::vect()

    if(divider == 2 ) {subgrid <- "d"}
    else if (divider == 4 ) {subgrid <- "c"}
    else if (divider == 5 ) {subgrid <- "p"}

    for(gr in g) {
      for(x1 in c(0:(divider-1))) {
        message(paste0("Generating grid for ",gr,x1,"..."))
        for(y1 in c(0:(divider-1))) {
          g0 = structure(c(
            grid_to_latlon(gr, xoffset = 0+y1/divider, yoffset = 0+x1/divider)[2],grid_to_latlon(gr, xoffset = (1+y1)/divider, yoffset = 0+x1/divider)[2],
            grid_to_latlon(gr, xoffset = (1+y1)/divider, yoffset = (1+x1)/divider)[2],grid_to_latlon(gr, xoffset = 0+y1/divider, yoffset = (1+x1)/divider)[2],
            grid_to_latlon(gr, xoffset = 0+y1/divider, yoffset = 0+x1/divider)[1],grid_to_latlon(gr, xoffset = (1+y1)/divider, yoffset = 0+x1/divider)[1],
            grid_to_latlon(gr, xoffset = (1+y1)/divider, yoffset = (1+x1)/divider)[1],grid_to_latlon(gr, xoffset = 0+y1/divider, yoffset = (1+x1)/divider)[1]),
            .Dim = c(4L, 2L))
          a <- terra::vect(g0, crs = "EPSG:4326", "polygons", atts = data.frame("Name" = paste0(gr,subgrid,x1,y1))) |>
            terra::project("EPSG:2180")
          if (dim(v)[1] == 0) {
            v <- a
          } else { v <- v+a }
        }
      }
    }
    v <- terra::project(v, "EPSG:2180") |>
      sf::st_as_sf()
    return(v)
  }
}
