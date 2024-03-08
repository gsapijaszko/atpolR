utils::globalVariables(c("atpol_square", "author_name", "description", "lat", "lon",
                         "number_of_entries", "record_number", "record_type", "st", ".get_all_data"))

#' extract_data_from_old_atpol() reads the ATPOL extracts from `.LOC` file and returns a tibble
#'
#' @importFrom tibble tibble
#' @importFrom dplyr rowwise mutate
#' @importFrom tidyr unnest
#' @importFrom utils read.delim
#' @param filename name of `.LOC` file
#' @return tibble with records
#' @export
#' @usage extract_data_from_old_atpol(filename = )
#' @examples
#' extract_data_from_old_atpol(filename = system.file("extdata/0200.LOC", package = "atpolR"))

extract_data_from_old_atpol <- function(filename = "") {
  header <- utils::read.delim(file = {{filename}},
                       skip = 0,
                       nrows = 4,
                       blank.lines.skip = FALSE,
                       header = FALSE)
  if(ncol(header) != 1L) {
    warning("More than one column in header!")
  }

  atpol_species_no <- sub(pattern = "Raport rekordow gatunku: ", replacement = "", header[1, 1]) |>
    as.numeric()
  species_name <- header[2, 1] |>
    as.character()
  no_of_unique_atpol_squares <- sub(pattern = "Liczba kwadratow: ", replacement = "", header[4,1]) |>
    as.numeric()

  if(no_of_unique_atpol_squares > 0L) {
    t <- utils::read.delim(file = {{filename}},
                    skip = 4,
                    fileEncoding = "CP852",
                    blank.lines.skip = FALSE,
                    header = FALSE)

    b <- tibble::tibble(
      lon = numeric(),
      lat = numeric(),
      record_type = character(),
      record_number = numeric(),
      atpol_square = character(),
      description = character(),
      number_of_entries = numeric(),
      author_name = character(),
      date = as.POSIXct(NA, "%Y%m%d", origin = "1970-01-01 00:00:00 UTC")
    )

    longlist <- lapply(seq_len(nrow(t)), function(i)  .get_all_data(st = t[i, ]))
    b <- do.call(rbind, longlist)
    b <- b |>
      tibble::as_tibble() |>
      dplyr::rowwise() |>
      dplyr::mutate(date = as.POSIXct(date, origin = "1970-01-01 00:00:00 UTC")) |>
      tidyr::unnest(cols = c(lon, lat, record_type, record_number, atpol_square, description, number_of_entries, author_name))

    b$species <- species_name
  } else {
    message({{filename}}, " seems to be empty")
    b <- NULL
  }
  return(b)
}

#' .get_all_data() parses string taken from ".LOC" file
#'
#' @importFrom stringr str_locate str_replace_all str_replace
#' @param {st} {string to decode}
#' @noRd
#'
.get_all_data <- function(st = "") {
  #  print(st)
  a <- list()

  dl_pos <- stringr::str_locate(st, "Dl:")
  dl <- substr(st, dl_pos[[1]], nchar(st))
  sz_pos <- stringr::str_locate(dl, "Sz:")
  sz <- trimws(substr(dl, sz_pos[[1]]+3, nchar(dl)))
  dl <- trimws(substr(dl, 4, sz_pos[[1]]-1))

  dl <- strsplit(dl, split = "(\\s)+")[[1]] |>
    as.numeric()

  sz <- strsplit(sz, split = "(\\s)+")[[1]] |>
    as.numeric()

  a$lon <- dl[[1]] + dl[[2]]/60 + dl[[3]]/60^2
  a$lat <- sz[[1]] + sz[[2]]/60 + sz[[3]]/60^2

  # sea <- stringi::stri_escape_unicode(c("ś", "ć", "ć", "Ĺ", "ĺ", "Ą", "ó", "×", "Ž", "ž", "Ć", "ť", "Ź", "í"))
  sea <- c("\\u015b", "\\u0107", "\\u0107", "\\u0139", "\\u013a", "\\u0104", "\\u00f3",
           "\\u00d7", "\\u017d", "\\u017e", "\\u0106", "\\u0165", "\\u0179", "\\u00ed")
  # rep <- stringi::stri_escape_unicode(c("Ś", "ą", "ć", "ę", "ł", "ń", "ó", "ś", "ź", "ż", "Ć", "Ł", "Ź", "Ż"))
  rep <- c("\\u015a", "\\u0105", "\\u0107", "\\u0119", "\\u0142", "\\u0144", "\\u00f3",
           "\\u015b", "\\u017a", "\\u017c", "\\u0106", "\\u0141", "\\u0179", "\\u017b")
  names(rep) <- sea

  atpol_pos <- stringr::str_locate(st, "[A-Z]{2}[0-9]{2,}")

  record_type_no <- trimws(substr(st, 1, atpol_pos[[1]]-1))
  record_type_no <- strsplit(record_type_no, "(\\s)+")

  if(length(record_type_no[[1]]) == 2L) {
    a$record_type <- record_type_no[[1]][1]
    a$record_number <- as.numeric(record_type_no[[1]][2])
  } else if(length(record_type_no[[1]]) == 1L) {
    if(grepl("^[A-Z][0-9]+", record_type_no[[1]][1]) &&  grepl("[0-9]$", record_type_no[[1]][1])) {
      a$record_type <- substr(record_type_no[[1]][1], 1, 1)
      a$record_number <- substr(record_type_no[[1]][1], 2, nchar(record_type_no[[1]][1])) |>
        as.numeric()
    }
  } else {
    a$record_type <- NA
    a$record_number <- NA
  }

  a$atpol_square <- substr(st, atpol_pos[[1]], atpol_pos[[2]])

  st <- trimws(substr(st, atpol_pos[[2]]+1, dl_pos[[1]]-1))

  desc_pos <- stringr::str_locate(st, pattern = "(\\d)+(\\s){2}[A-Z]")

  a$description <- trimws(substr(st, 1, desc_pos[[1]]-1)) |>
    stringr::str_replace_all(pattern = "(\\s){1,}", replacement = " ") |>
    stringr::str_replace_all(pattern = rep) |>
    stringr::str_replace(pattern = "(\\,)$", replacement = "")

  year_pos <- stringr::str_locate(st, "(\\d){1,}$")
  year <- substr(st, year_pos[[1]], year_pos[[2]])
  if(year == "0") {
    a$date <- as.POSIXct(NA, "%Y%m%d", origin = "1970-01-01 00:00:00 UTC")
  } else if(nchar(year) == 4L) {
    a$date <- as.POSIXct(paste0(year, "-01-02 00:00:00 UTC"), origin = "1970-01-01 00:00:00 UTC")
  }

  st <- trimws(substr(st, desc_pos[[1]], year_pos[[1]]-1))
  number_pos <- stringr::str_locate(st, "^(\\d){1,}")

  a$number_of_entries <- substr(st, number_pos[[1]], number_pos[[2]]) |>
    as.numeric()
  a$author_name <- trimws(substr(st, number_pos[[2]]+1, nchar(st))) |>
    stringr::str_replace_all(pattern = "(\\s){1,}", replacement = " ") |>
    stringr::str_replace_all(pattern = rep)

  return(a)
}
