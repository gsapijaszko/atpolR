# .myCols defines a colors used for classified raster colorization
.myCols <- c("#00A600",
           "#29AB08",
           "#4CBF00",
           "#7ACC00",
           "#ADD900",
           "#FFFF00",
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

#' .atpolBackground prepares and plots the map of ATPOL big grid (100km x 100km) on rasterized Poland
#' @importFrom sf st_bbox
#' @importFrom terra plot rast
#' @importFrom graphics axis par
#' @param main image title, usually a species name
#' @param colors vector of colors to be used as a background, default internal .myCols
#' @noRd
#'
.atpolBackground <- function(main = "", colors = .myCols){
  .bbox <- sf::st_bbox(atpol10k())
  cr <- terra::rast(system.file("extdata/cr.tif", package = "atpolR"))
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(pty = "s")
  terra::plot(cr, type="classes",
              col = colors,
              #        col = terrain.colors(12, alpha = 1, rev = FALSE),
              legend = FALSE,
              xlim = c(.bbox[1], .bbox[3]),
              ylim = c(.bbox[2], .bbox[4]),
              axes = FALSE,
              main = {{main}}
  )
  # adding water layer
  w <- terra::rast(system.file("extdata/water.tif", package = "atpolR"))
  terra::plot(w, add = TRUE, col = "#4476C3", legend = FALSE, axes = FALSE)

  #
  a100k <- atpol100k()
  terra::plot(a100k$geometry, lwd = 0.5, add= TRUE)
  terra::plot(boundaryPL(), col = "darkred", add = TRUE)
  d <- list()
  for (letter in LETTERS[1:7]) {
    a <- subset(a100k, a100k$Name == paste0("D", letter)) |> sf::st_bbox()
    c <- a[4] - (a[4] - a[2])/2
    d <- append(d, c)
  }
  axis(2, at = c(d[1:7]), labels = c("A", "B", "C", "D", "E", "F", "G"), las = 1, lwd = 0, lwd.ticks = 0, line = -2)
  axis(4, at = c(d[1:7]), labels = c("A", "B", "C", "D", "E", "F", "G"), las = 1, lwd = 0, lwd.ticks = 0, line = -2)

  d <- list()
  for (letter in LETTERS[1:7]) {
    a <- subset(a100k, a100k$Name == paste0(letter, "D")) |> sf::st_bbox()
    c <- a[3] - (a[3] - a[1])/2
    d <- append(d, c)
  }
  axis(1, at = c(d[1:7]), labels = c("A", "B", "C", "D", "E", "F", "G"), las = 1, lwd = 0, lwd.ticks = 0, line = -2)
  axis(3, at = c(d[1:7]), labels = c("A", "B", "C", "D", "E", "F", "G"), las = 1, lwd = 0, lwd.ticks = 0, line = -2)
}

#' plotPoitsOnAtpol() plots the observations on ATPOL 10km x 10km grid
#'
#' @importFrom grDevices dev.off png svg
#' @importFrom terra plot
#' @param myData SimpleFeature data frame with point geometry, usually centroid of ATPOL grid square
#' @param outputType image output type, either "svg" or "png"; if not specified a standard output device is used (screen)
#' @param filename name of the output file
#' @param main image title, usually a species name
#' @param colors vector of colors to be used as a background, default internal .myCols
#' @param cex size of the points, default 0.9
#' @param col color of the points, default black
#' @param pch shape of the point, default 16 - filled dot
#' @return choreograph map of species distribution in Poland.
#' @export
#' @usage plotPoitsOnAtpol(myData, outputType, filename, main, colors, cex, col, pch)

plotPoitsOnAtpol <- function(myData = "", outputType = "", filename = "", main = "", colors = .myCols, cex = 0.9, col = "black", pch = 16) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  if(nzchar({{filename}}) == TRUE) {
    if(outputType == "svg") {
      grDevices::svg(file=paste0({{filename}}, ".svg"),width=12,height=12)
    } else if(outputType == "svg"){
      grDevices::png(file=paste0({{filename}}, ".png"), width=1600, height=1600, res=240)
    }
    else {
      warning("Please provide an outputType (svg or png).")
    }
  }
  .atpolBackground(main = {{main}}, colors = {{colors}})
  terra::plot({{myData}}, pch = {{pch}}, cex = ifelse(outputType == "svg", 1.5, {{cex}}), add = TRUE, col = {{col}}, lty = 1)
  if(nzchar({{filename}}) == TRUE && nzchar({{outputType}}) == TRUE) {
    grDevices::dev.off()
  }
}
