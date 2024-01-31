# .myCols defines a colors used for classified raster colorization
.myCols <- c("#56A54F",
             "#6EB959",
             "#8DC762",
             "#B5D56D",
             "#FDFD87",
             "#F7DE79",
             "#F2BB6A",
             "#ED985D",
             "#EA7952",
             "#E85E4A",
             "#CE5148",
             "#E74A45",
             "#E04141",
             "#BC3636",
             "#9A2A2A")

#' plotPoitsOnAtpol() plots the observations on ATPOL 10km x 10km grid
#'
#' @importFrom grDevices dev.off png svg
#' @importFrom terra plot rast
#' @importFrom sf st_bbox
#' @importFrom graphics axis par
#' @param myData SimpleFeature data frame with point geometry, usually centroid of ATPOL grid square
#' @param outputType image output type, either "svg" or "png"; if not specified a standard output device is used (screen)
#' @param filename name of the output file
#' @param main image title, usually a species name
#' @param colors vector of colors to be used as a background, default internal .myCols. If FALSE, black and white plot has to be expected
#' @param water logical, if water layer has to be included, default TRUE
#' @param cex size of the points, default 0.9
#' @param col color of the points, default black
#' @param pch shape of the point, default 16 - filled dot
#' @param grid10k logical, default FALSE. If the small grid has to be printed out
#' @return choreograph map of species distribution in Poland.
#' @export
#' @usage plotPoitsOnAtpol(myData, outputType, filename, main, colors,
#' water, cex, col, pch, grid10k)

plotPoitsOnAtpol <- function(myData = "", outputType = "", filename = "", main = "",
                             colors = .myCols, water = TRUE,
                             cex = 0.9, col = "black", pch = 16, grid10k = FALSE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  if(nzchar({{filename}}) == TRUE) {
    if(outputType == "svg") {
      grDevices::svg(file=paste0({{filename}}, ".svg"),width=12,height=12)
    } else if(outputType == "png"){
      grDevices::png(file=paste0({{filename}}, ".png"), width=1600, height=1600, res=240)
    }
    else {
      warning("Please provide an outputType (svg or png).")
    }
  }
  ### generate background
  .bbox <- sf::st_bbox(atpol10k())
  par(pty = "s")
  # a100k <- atpol100k()
  a100k <- setNames(aggregate(.atpol10k_full()[,"geometry"], by = list(substr(.atpol10k_full()$Name, 1, 2)), mean), c("Name", "geometry"))
  cr <- terra::rast(system.file("extdata/cr.tif", package = "atpolR"))
  if(!isFALSE({{colors}})) {
    terra::plot(cr, type="classes",
                col = colors,
                #        col = terrain.colors(12, alpha = 1, rev = FALSE),
                legend = FALSE,
                xlim = c(.bbox[[1]], .bbox[[3]]),
                ylim = c(.bbox[[2]], .bbox[[4]]),
                axes = FALSE,
                main = {{main}})

    if(isTRUE({{water}})) {
      # adding water layer
      w <- terra::rast(system.file("extdata/water.tif", package = "atpolR"))
      terra::plot(w, add = TRUE, col = "#4476C3", legend = FALSE, axes = FALSE)
    }
  } else {
    # although it doesn't make sense to plot white raster,
    # it's to have the same plot dimension
    terra::plot(cr, type="classes",
                col = "white",
                legend = FALSE,
                xlim = c(.bbox[[1]], .bbox[[3]]),
                ylim = c(.bbox[[2]], .bbox[[4]]),
                axes = FALSE,
                main = {{main}})
  }
  terra::plot(a100k$geometry, lwd = 0.3, add= TRUE)

  d <- list()
  for (letter in LETTERS[1:7]) {
    a <- subset(a100k, a100k$Name == paste0("D", letter)) |> sf::st_bbox()
    c <- a[4] - (a[4] - a[2])/2
    d <- append(d, c)
  }
  axis(2, ## left
       at = c(d[1:7]), labels = c("A", "B", "C", "D", "E", "F", "G"),
       pos = .bbox[[1]],
       las = 1, lwd = 0, lwd.ticks = 0, line = -0.5, cex = 1.1)

  # axis(4, ## right
  #      at = c(d[1:7]),
  #      pos = .bbox[[3]],
  #      labels = c("A", "B", "C", "D", "E", "F", "G"),
  #      las = 1, lwd = 0, lwd.ticks = 0, line = -0.5, cex = 1.1)

  d <- list()
  for (letter in LETTERS[1:7]) {
    a <- subset(a100k, a100k$Name == paste0(letter, "D")) |> sf::st_bbox()
    c <- a[3] - (a[3] - a[1])/2
    d <- append(d, c)
  }

  # axis(1, ## below
  #      at = c(d[1:7]),
  #      pos = .bbox[[2]],
  #      labels = c("A", "B", "C", "D", "E", "F", "G"),
  #      las = 1, lwd = 0, lwd.ticks = 0, line = -0.5, cex = 1.1)

  axis(3, ## above
       at = c(d[1:7]),
       labels = c("A", "B", "C", "D", "E", "F", "G"),
       pos = .bbox[[4]],
       las = 1, lwd = 0, lwd.ticks = 0, line = -0.5, cex = 1.1)

  if(isTRUE({{grid10k}})) {
    gr <- atpol10k()
    gr |>
      sf::st_geometry() |>
      sf::st_cast(to = "LINESTRING") |>
      terra::plot(lwd = 0.2, col = "grey30", add = TRUE)
  }

  if(isFALSE({{colors}}) & isFALSE({{water}})) {
    suppressMessages(sf::st_read(system.file("extdata/waters.gpkg", package = "atpolR"), layer = "rivers")) |>
      sf::st_geometry() |>
      terra::vect() |>
      terra::plot(add = TRUE, col = "blue")

    suppressMessages(sf::st_read(system.file("extdata/waters.gpkg", package = "atpolR"), layer = "water")) |>
      sf::st_geometry() |>
      terra::vect() |>
      terra::plot(col = "blue", border = "blue", add = TRUE)
  }

  terra::plot(terra::vect(boundaryPL()), col = "darkred", add = TRUE)

  ### adding data
  if(inherits({{myData}}, "sf") || inherits({{myData}}, "sfc")) {
    myData <- terra::vect(myData)
    terra::plot({{myData}}, pch = {{pch}},
                cex = ifelse(outputType == "svg", 1.5, {{cex}}),
                add = TRUE, col = {{col}}, lty = 1)

  }

  if(nzchar({{filename}}) == TRUE && nzchar({{outputType}}) == TRUE) {
    grDevices::dev.off()
  }
}
