# preparation of basic 10km x 10km grid used in other functions and saving it as data file
# the data is in terra::vect format
if(!file.exists("inst/extdata/atpol10k.Rds")) {
  v <- terra::vect()
  for (X in LETTERS[1:7]) {
    for (Y in LETTERS[1:7]) {
      print(paste0("generating grids for ", X,Y))
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
# terra::plot(cr)
# terra::plot(atpol100k()$geometry, add=TRUE)


myCol <- c("#00A600",
           "#29AB08",
           #           "#24B300",
           "#4CBF00",
           "#7ACC00",
           "#ADD900",
           "#FFFF00", # zolty
           "#FFDD00",
           "#FFB700",
           "#FF9000",
           "#FF6A00",
           "#FF4400",
           "#E23924",
           "#FF1D00",
           "#F70000",
           "#D00000",
           "#AA0000")


v <- terra::vect(readRDS(system.file("extdata", "atpol10k.Rds", package = "atpolR", mustWork = TRUE)))
v <- sf::st_as_sf(v)
v

boundary <- geodata::gadm(country = "POL", level = 1, path = tempdir(), version = "3.6")
boundary <- sf::st_as_sf(boundary)
sf::st_crs(boundary) <- 4326
boundary <- sf::st_union(boundary) |>
#   sf::st_cast(to = "MULTILINESTRING") |>
  sf::st_as_sf() |>
  sf::st_transform(crs = "EPSG:2180")

v$intersects <- sf::st_intersects(v, boundary, sparse = FALSE)
subset(v, v$intersects == TRUE)

# a<- atpol10k()
# aggregate(atpol10k()[,"geometry"], by = list(substr(atpol10k()$Name, 1, 2)), sum)
#
#
# a[,"geometry"]
# substr(a$Name, 1, 2)

# .bbox <- sf::st_bbox(atpol10k())
# cr <- terra::rast("inst/extdata/cr.tif")
# w <- cr
# terra::values(w) <- NA
#
# wody <- sf::st_read("/home/sapi/projekty/atpol/data/wody.shp") |>
# #  dplyr::filter(way_area > 750000) |>
#   sf::st_transform(crs = "EPSG:2180")
#
# terra::plot(wody, add=TRUE)
# object.size(wody)
# # |>
#   dplyr::select(geometry) |>
#   plot(col = "#4476C3", border = "#4476C3", add = TRUE)
#
# x<- terra::rasterize(terra::vect(wody), w, touches = TRUE)
# terra::writeRaster(x, filename = "inst/extdata/water.tif")
#
# terra::plot(cr, type="classes",
#             col = myCol,
#             #        col = terrain.colors(12, alpha = 1, rev = FALSE),
#             legend = FALSE,
#             xlim = c(.bbox[1], .bbox[3]),
#             ylim = c(.bbox[2], .bbox[4]),
#             axes = FALSE,
#             main = ""
# )
# terra::plot(x, add = TRUE, col = "#4476C3", legend = FALSE)
#
# terra::plot(atpolR::atpol100k()$geometry, lwd = 0.5, add= TRUE)
# terra::plot(atpolR::boundaryPL(), col = "darkred", add = TRUE)
# # b <- boundaryPL()
# # bs <- sf::st_simplify(boundaryPL(), dTolerance = 100)
# # base::saveRDS(bs, file = "inst/extdata/pl_boundary.Rds")
# # object.size(bs)
# # terra::saveRDS(terra::vect(bs), file = "inst/extdata/pl_boundary.Rds")
# # base::readRDS("inst/extdata/pl_boundary.Rds")

# t <- atpol10k() |>
#  dplyr::filter(Name %in% c("BF03", "BF04", "BF13"))
# plotPoitsOnAtpol(t$centroid)

# f <- "/home/sapi/projekty/stforek/widlaki/source/atpolscan/Diph-alp_0059_modified.tif"
# grepl("ID\\[\"EPSG\",2180\\]\\]",terra::crs(terra::rast(f)))
# sf::st_crs(terra::rast(f))[[1]] == "ETRS89 / Poland CS92"
# terra::crs(
#   terra::crop(terra::rast(f), terra::vect(atpol10k()))
# )
