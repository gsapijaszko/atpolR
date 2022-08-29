# preparation of basic 10km x 10km grid used in other functions and saving it as data file
# the data is in terra::vect format
if(!file.exists("inst/extdata/atpol10k.Rds")) {
  v <- terra::vect()
  for (X in LETTERS[1:7]) {
    for (Y in LETTERS[1:7]) {
      message(paste0("generating grids for ", X,Y))
      for(x1 in c(0:9)) {
        for(y1 in c(0:9)) {
          g0 = structure(c(
            grid_to_latlon(paste0(X,Y,x1,y1), 0, 0)[2],grid_to_latlon(paste0(X,Y,x1,y1), 0, 1)[2],
            grid_to_latlon(paste0(X,Y,x1,y1), 1, 1)[2],grid_to_latlon(paste0(X,Y,x1,y1), 1, 0)[2],
            grid_to_latlon(paste0(X,Y,x1,y1), 0, 0)[1],grid_to_latlon(paste0(X,Y,x1,y1), 0, 1)[1],
            grid_to_latlon(paste0(X,Y,x1,y1), 1, 1)[1],grid_to_latlon(paste0(X,Y,x1,y1), 1, 0)[1]),
            .Dim = c(4L, 2L))
          a <- terra::vect(g0, crs = "EPSG:4326", "polygons", atts = data.frame("Name" = paste0(X,Y,x1,y1))) |>
            terra::project("EPSG:2180")

          if (dim(v)[1] == 0) {
            v <- a
          } else { v <- v+a}
        }
      }
    }
  }
  v <- terra::project(v, "EPSG:2180")
  terra::saveRDS(v, file = "inst/extdata/atpol10k.Rds")
}

# preparing the simplified boundary data of Poland
if(!file.exists("inst/extdata/pl_boundary.Rds")) {
  # getting PL boundary data
  boundary <- geodata::gadm(country = "POL", level = 1, path = tempdir(), version = "3.6")
  boundary <- sf::st_as_sf(boundary)
  sf::st_crs(boundary) <- 4326
  boundary <- sf::st_union(boundary) |>
    sf::st_cast(to = "MULTILINESTRING") |>
    sf::st_as_sf() |>
    sf::st_transform(crs = "EPSG:2180") |>
    sf::st_simplify(dTolerance = 100)
  base::saveRDS(boundary, file = "inst/extdata/pl_boundary.Rds")
}


if(!file.exists("inst/extdata/cr.tif")) {
  # preparing elevation raster for Poland with BBox of ATPOL
  # CRS = EPSG:2180
  countries <- c("POL", "CZE", "SVK", "UKR", "BLR", "LTU")
  for (cntry in countries) {
    geodata::elevation_30s(country = cntry, path = tempdir(), mask = FALSE)
  }
  .bbox <- sf::st_bbox(atpol10k())
  .ext <- terra::ext(c(.bbox[1], .bbox[3], .bbox[2], .bbox[4]))

  r <- list.files(path = tempdir(), pattern = ".+._elv.tif", full.names = TRUE)
  r <- lapply(r, terra::rast)
  r <- do.call(terra::merge, r)
  terra::crs(r) <- "EPSG:4326"
  r<-terra::project(r, "EPSG:2180") |>
    terra::crop(r, .ext) |>
    terra::classify(r, c(-30,10,150,250,350,500,600, 700, 800, 1000, 1200, 1500, 1700, 2000, 2500)) |>
    terra::writeRaster(r, filename = "inst/extdata/cr.tif")
}
