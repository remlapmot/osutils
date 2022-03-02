# Functions for managing codelists ----


#' Converts a json file of codelist names and URLs into an HTML table
#'
#' @param import_json_from A character containing the path of the json file containing the codelists.
#'   defaults to `./codelists/codelists.json` which is the OpenSAFELY standard
#' @param export_to The path to which the file should be saved
#'
#' @details This function currently only exports an HTML file but it can be adapted to output text, markdown, etc.
#' Ideally this would be an in-build OpenSAFELY feature rather than written externally in R.
#' @export
reformat_codelists <- function(
    import_json_from = "./codelists/codelists.json",
    export_to
  ){

  # import codelists from json
  codelists <- jsonlite::read_json(
    path=import_json_from,

  )


  # reformat
  codelists_formatted <- tibble::enframe(codelists[[1]])
  codelists_formatted <- tidyr::unnest_wider(codelists_formatted, value)
  codelists_formatted <-
    dplyr::mutate(
      codelists_formatted,
      file = name,
      name= stringr::str_extract(id, "(?<=/)(.+)(?=/)"),
      downloaded_at = as.Date(downloaded_at, "%Y-%m-%d")
    )

  # output to html
  gt::gt(codelists_formatted[,c("name", "url", "downloaded_at")]) %>%
    gt::cols_label(
      name = "Name",
      url = "URL",
      downloaded_at = "Accessed on"
    ) %>%
    gt::gtsave(
      export_to
    )
}


#' Indicates which values to redact from a vector of frequencies
#'
#' @param n A vector of integer frequencies or counts from a 1-dimension frequency distribution.
#' @param threshold The redaction threshold. All values (and possibly more; see details) less than or equal to this threshold will be redacted.
#' @return A logical vector the same length as `n`.
#'
#' @details Given a vector of frequencies `n`, this function returns a logical vector of frequencies to be redacted.
#'  All frequencies less than or equal to the threshold are redacted.
#'  If the sum the redacted frequencies is also less than or equal to the threshold, then the smallest unredacted frequency is also redacted.
#' @export
redactor <- function(n, threshold){

  stopifnot("non-integer values passed to n" = all(n%%1 == 0L))
  stopifnot("threshold must be a scalar" = length(threshold) == 1L)
  stopifnot("non-integer value passed to threshold" = threshold%%1 == 0L)
  stopifnot("n must be non-missing" = all(!is.na(n)))
  stopifnot("n must non-negative" = all(n>=0))

  n <- as.integer(n)
  leq_threshold <- dplyr::between(n, 1, threshold)
  n_sum <- sum(n)

  # redact if n is less than or equal to redaction threshold
  redact <- leq_threshold

  # also redact the smallest unredacted value if the sum of redacted n is still less than or equal to threshold
  if((sum(n*leq_threshold) <= threshold) & any(leq_threshold)){
    redact[which.min(dplyr::if_else(leq_threshold, n_sum+1L, n))] = TRUE
  }

  # return
  redact
}
