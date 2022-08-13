# All functions based on https://sourceforge.net/p/atpol/code/HEAD/tree/
# 19E, 52N

.A52 = 0.907571211037051
.RAD_TO_DEG <- 57.2957795130823
.EARTH_RADIUS <- 6390
.LAMBDA_ZERO <- 19
.X_ZERO <- 330
.Y_ZERO <- 350

.latlon_to_xy <- function(lat, lon) {
  lon = lon - .LAMBDA_ZERO
  lon = lon / .RAD_TO_DEG
  lat = lat / .RAD_TO_DEG
  r = cos(.A52) / sin(.A52) - tan(lat - .A52)
  x = r * sin(lon * sin(.A52))
  y = r * cos(lon * sin(.A52))
  y = y - cos(.A52) / sin(.A52)
  x = x * .EARTH_RADIUS + .X_ZERO
  y = y * .EARTH_RADIUS + .Y_ZERO
  a <- c(x,y)
  return(a)
}

.xy_to_latlon <- function(x, y) {
  x = (x - .X_ZERO) / .EARTH_RADIUS
  y = (y - .Y_ZERO) / .EARTH_RADIUS
  y = y + cos(.A52) / sin(.A52)
  lon = atan(x / y) / sin(.A52)
  r = sqrt(x * x + y * y)
  lat = .A52 - atan(r - cos(.A52) / sin(.A52))
  lat = lat * .RAD_TO_DEG
  lon = lon * .RAD_TO_DEG + .LAMBDA_ZERO
  a <- c(lat, lon)
  return(a)
}

.xy_to_grid <- function (x, y, len) {
  grid = ""
  xoffset = 0;
  yoffset = 0;
  if ((x >= 0) & (x < 700) & (y >= 0) & (y < 700) & (len >= 2) & (len <= 12) & ((len %% 2) == 0)) {
    xs = round(x * 1000);
    ys = round(y * 1000);
    xs = as.character(xs);
    ys = as.character(ys);
    xs = stringr::str_pad(xs, 6, '0', side = "left");
    ys = stringr::str_pad(ys, 6, '0', side = "left");
    grid = paste0(intToUtf8(utf8ToInt(substr(xs,1,1))+17),intToUtf8(utf8ToInt(substr(ys,1,1))+17))
    for (i in 2:6) {
      grid = paste0(grid, substr(ys,i,i),substr(xs,i,i));
    }
    xoffset = as.character(as.integer(round(x * 1000000)))
    yoffset = as.character(as.integer(round(y * 1000000)))
    xoffset = stringr::str_pad(xoffset, 9, '0', side = "left");
    yoffset = stringr::str_pad(yoffset, 9, '0', side = "left");
    xoffset = paste0("0.", substr(xoffset, -9 + len / 2, 9))
    yoffset = paste0("0.", substr(yoffset, -9 + len / 2, 9))
    xoffset = as.double(xoffset);
    yoffset = as.double(yoffset);
  }
  a <- c(substr(grid, 0, len), xoffset, yoffset)
  return(a)
}

.grid_to_xy <- function(grid, xoffset, yoffset) {
  grid = toupper(grid);
  l = nchar(grid);
  if (l > 0 & grepl('^[A-G][A-G][0-9]*',grid)) {
    xs = intToUtf8(utf8ToInt(substr(grid,1,1))-17);
    ys = intToUtf8(utf8ToInt(substr(grid,2,2))-17);
    if (l/2 > 1){
      for (i in 2:(l/2)) {
        xs = paste0(xs, substr(grid, 2*i, 2*i))
        ys = paste0(ys, substr(grid, 2*i-1, 2*i-1))
      }
    }
    if(l/2 < 6) {
      for (i  in ((l/2)+1):6) {
        xs = paste0(xs, "0")
        ys = paste0(ys, "0")
      }
    }
    x <- as.numeric(xs)/1000 + 10^(3-l/2) * as.numeric(xoffset);
    y <- as.numeric(ys)/1000 + 10^(3-l/2) * as.numeric(yoffset);
  }
  a <- c(x, y)
  return(a);
}

#' grid_to_latlon converts the ATPOL grid to latitude and longitude.
#' With \code{xoffset = 0} and \code{yoffset = 0} parameters it returns coordinates of the upper left corner of the grid.
#' @param grid An ATPOL grid, ex. "GF2345".
#' @param xoffset An offset in X, where 0 is for left, and 1 for right side of the grid. The default value is 0.5, which corresponds to middle of the grid.
#' @param yoffset An offset in Y, where 0 is for top, and 1 for bottom side of the grid. The default value is 0.5, which corresponds to middle of the grid.
#' @return latitude and longitude of ATPOL grid (default centroid) as pair of numerics
#' @examples
#' grid_to_latlon("BE21")
#' grid_to_latlon("BE21", 0, 0)
#' @references \url{https://atpol.sourceforge.io/}
#' @export
#'
grid_to_latlon <- function(grid, xoffset=0.5, yoffset=0.5) {
  xy = .grid_to_xy(grid, xoffset, yoffset)
  latlon = .xy_to_latlon(xy[1], xy[2]);
  return(latlon)
}

#' latlon_to_grid(lat, lon, length) converts geographical coordinates to ATPOL grid of given length
#' @import stringr
#' @param lat Latitude in degrees, ex. 51.123456
#' @param lon Longitude in degrees, ex. 17.234567
#' @param length Desired ATPOL grid length, which can be 2, 4, 6, 8, 10 or 12,
#' @return grid, ex. BE, BE23, BE2357, etc.
#' @examples latlon_to_grid(51, 17, 2)
#' @examples latlon_to_grid(51, 17, 6)
#' @references \url{https://atpol.sourceforge.io/}
#' @export
latlon_to_grid <- function(lat, lon, length) {
  xy = .latlon_to_xy(lat, lon);
  grid = .xy_to_grid(xy[1], xy[2], length);
  return(grid[1])
}

# a<- latlon_to_grid(50,16, 4)
# grid_to_latlon("BF61", 0, 0)
