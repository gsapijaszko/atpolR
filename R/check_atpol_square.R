#' Reverse engineering of published ATPOL grids
#'
#' checkATPOLSquare() do a reverse enginnering of published ATPOL grids species,
#' especially those published in: Zając, Adam, and Maria Zając, eds. 2001.
#' Atlas rozmieszczenia roślin naczyniowych w Polsce. Distribution Atlas of Vascular Plants in Poland.
#' Kraków: Laboratory of Computer Chorology - Institute of Botany - Jagiellonian University.
#' @importFrom sf st_buffer
#' @importFrom terra crop vect
#' @param centroid SF point geometry for which the check is performed,
#' usually it corresponds to centroid of ATPOL 10km x 10km grid
#' @param raster geocoded raster, it has to be in EPSG:2180 projection
#' @param distance st_buffer distance from centroid point for which the check is done, default 1200 m
#' @returns "YES" or "?" for given SF point
#'
#' @usage checkATPOLSquare(centroid, raster, distance)
#' @export
#'
checkATPOLSquare <- function(centroid, raster, distance = 1200) {
  if (!grepl("ID\\[\"EPSG\",2180\\]\\]",terra::crs({{raster}}))) {
    print("Raster has to be in EPSG:2180 projection")
    print("Try terra::project(raster, \"EPSG:2180\")")
    stop()
  }
  c <- sf::st_buffer({{centroid}}, dist = {{distance}})
  mm <- terra::minmax(terra::crop({{raster}}, terra::vect(c)))
  y <- ifelse(mm[1] == 0 & mm[2] == 0, "YES", "?")
  return(y)
}
