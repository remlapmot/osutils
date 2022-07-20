# Functions for saving and loading OS-compliant file types ----


#' Write a data frame to a delimited file, and save typing information in a separate json file
#'
#' @param x A data frame or tibble to write to disk.
#' @param path File or connection to write to. (path is now deprecated in readr v1.4 for OpenSAFELY currently has older version, so use path for now)
#' @param suffix The suffix used in the name of the json file, to be appended to the delimited file name. Defaults to `""` (no suffix), so that the file name is the same as the delimited file name (excluding filetype extensions).
#' @param delim Delimiter used to separate values.
#' @param na String used for missing values. Defaults to NA. Missing values will never be quoted; strings with the same value as na will always be quoted.
#' @param quote_escape The type of escaping to use for quoted values, one of "`double`", "`backslash`" or "`none`". You can also use `FALSE`, which is equivalent to "`none`". The default is "`double`", which is expected format for Excel.
#' @param eol The end of line character to use. Most commonly either "`\n`" for Unix style newlines, or "`\r\n`" for Windows style newlines.
#' @return Returns the input invisibly
#'
#' @details Based on the [readr::write_delim] function.
#'  Additionally, this function saves a json file containing typing info for the data frame,
#'  which can be used to re-type the data when re-imported into R.
#'  Some further read_delim options are deliberately unavailable as they won't make sense for files intended for re-importing.
#'  Datetime and time classes are not supported.
#' @export

writetype_delim <- function(
    x,
    path, ## note OS readr version uses old `path` argument not  `file`, so use `path` for compatibility
    suffix = "",
    delim = " ",
    na = "NA",
    quote_escape = "double",
    eol = "\n"
){


  concise_lookup <- tibble::tribble(
    ~concise, ~col_type, ~class, ~type,

    "c", "character", "character", "character",
    "f", "factor", "factor", "integer",
    "d", "double", "numeric", "double",
    "i", "integer", "integer", "integer",
    "l", "logical", "logical", "logical",
    #"n",  "number", NA, NA
    "D", "date", "Date", "double",
    #"T", "datetime", NA, "double"
    #"t", "time", NA, "double"
  )

  x_type <-
    tibble::tibble(
      col_name = names(x),
      class = purrr::map_chr(x, class),
      type = purrr::map_chr(x, typeof),
      attributes = purrr::map(x, attributes),
      levels = purrr::map(x, ~ levels(.) ),
      concise = concise_lookup$concise[match(class, concise_lookup$class)],
      col_type = concise_lookup$col_type[match(class, concise_lookup$class)]
    )

  jsonpath <- paste0(fs::path_ext_remove(path), suffix, ".json")

  jsonlite::write_json(x_type, path=jsonpath, pretty=TRUE)


  readr::write_delim(
    x=x,
    path=path,
    delim=delim,
    na=na,
    append=FALSE,
    col_names=TRUE,
    quote_escape=quote_escape,
    eol=eol
  )

}

#' Write a data frame to a csv file, and save typing information in a separate json file
#'
#' @param x A data frame or tibble to write to disk.
#' @param path File or connection to write to. (path is now deprecated in readr v1.4 for OpenSAFELY currently has older version, so use path for now).
#' @param suffix The suffix used in the name of the json file, to be appended to the delimited file name. Defaults to `""` (no suffix), so that the file name is the same as the delimited file name (excluding filetype extensions).
#' @param na String used for missing values. Defaults to NA. Missing values will never be quoted; strings with the same value as na will always be quoted.
#' @param quote_escape The type of escaping to use for quoted values, one of "`double`", "`backslash`" or "`none`". You can also use `FALSE`, which is equivalent to "`none`". The default is "`double`", which is expected format for Excel.
#' @param eol The end of line character to use. Most commonly either "`\n`" for Unix style newlines, or "`\r\n`" for Windows style newlines.
#' @return Returns the input invisibly
#'
#' @details Based on the readr::write_delim function.
#'  Additionally, this function saves a json file containing typing info for the data frame,
#'  which can be used to re-type the data when re-imported into R.
#'  Datetime and time classes are not supported.
#' @export

writetype_csv <- function(
    x,
    path, ## note OS readr version uses old `path` argument not  `file`, so use `path` for compatibility
    suffix = "",
    na = "NA",
    quote_escape = "double",
    eol = "\n"
){

  writetype_delim(
    x=x,
    path=path, ## note OS readr version uses old `path` argument not  `file`, so use `path` for compatibility
    suffix=suffix,
    delim=",",
    na=na,
    quote_escape=quote_escape,
    eol=eol
  )

}


#' Read a delimited file (including CSV and TSV) into a tibble, and type columns using a separate json file
#'
#' @param file Delimited file location.
#' @param suffix #' @param suffix The suffix used in the name of the json file, which is appended to the delimited file name. Defaults to `""` (no suffix), so that the file name is the same as the delimited file name (excluding filetype extensions).
#' @details Based on the readr::read_delim function. Requires delimited files to be saved using osutils::write_delim, which will also create the json file containing the typing info.
#'  See readr::read_csv for additional parameter details
#'  Datetime and time classes are not supported.
#' @export

readtype_delim <- function(
    file,
    suffix = "",
    delim,
    quote = "\"",
    escape_backslash = FALSE,
    escape_double = TRUE,
    locale = default_locale(),
    na = c("", "NA"),
    quoted_na = TRUE,
    comment = "",
    trim_ws = FALSE
){

  jsonpath <- paste0(fs::path_ext_remove(file), suffix, ".json")

  x_type <- jsonlite::read_json(jsonpath) %>%
    tibble::enframe(name=NULL) %>%
    tidyr::unnest_wider(value) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      #col_spec = list(get(paste0("col_",col_type))),
      levels = list(unlist(levels)) # to change from list of lists to list of character vectors
    )

  x <- readr::read_delim(
    file = file,
    delim = delim,
    col_types = paste(x_type$concise, collapse=""),
    quote = quote,
    escape_backslash = escape_backslash,
    escape_double = escape_double,
    locale = locale,
    na = na,
    quoted_na = quoted_na,
    comment = comment,
    trim_ws = trim_ws
  )

  factors <- dplyr::filter(x_type, class=="factor")$col_name

  for (fc in factors) {
    levels <- dplyr::filter(x_type, col_name==fc)$levels[[1]]
    x[[fc]] <- factor(x[[fc]], levels = levels)
  }

  x

}


#' Read a csv file into a tibble, and type columns using a separate json file
#'
#' @param file Delimited file location.
#' @param suffix #' @param suffix The suffix used in the name of the json file, which is appended to the delimited file name. Defaults to `""` (no suffix), so that the file name is the same as the delimited file name (excluding filetype extensions).
#' @details Based on the readr::read_csv function. Requires csv  files to be saved using osutils::write_csv, which will also create the json file containing the typing info.
#'  See readr::read_csv for additional parameter details
#'  Datetime and time classes are not supported.
#' @export

readtype_csv <- function(
  file,
  suffix = "",
  delim,
  quote = "\"",
  escape_backslash = FALSE,
  escape_double = TRUE,
  locale = default_locale(),
  na = c("", "NA"),
  quoted_na = TRUE,
  comment = "",
  trim_ws = FALSE
){
  readtype_delim(
    file = file,
    suffix = suffix,
    delim = ",",
    quote = quote,
    escape_backslash = escape_backslash,
    escape_double = escape_double,
    locale = locale,
    na = na,
    quoted_na = quoted_na,
    comment = comment,
    trim_ws = trim_ws
  )
}
