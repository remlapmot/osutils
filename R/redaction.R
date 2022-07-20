# Functions for redacting vectors ----

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

#' Redact values in a vector based on frequency values
#'
#' @param n A vector of integer frequencies or counts from a 1-dimension frequency distribution.
#' @param threshold The redaction threshold. All values (and possibly more; see details) less than or equal to this threshold will be redacted.
#' @param x Values to redact. If `x` is `NULL` then `x` redacts values of `n`.
#' @return A vector the same length as `n`.
#'
#' @details If `x` is `NULL`, then this function redacts values in `n` and returns the redacted vector.
#'  If `x` is not `NULL`, values in `x` are redacted according to frequencies in `n`.
#'  Values are redacted as follows:
#'    all frequencies less than or equal to the threshold are redacted;
#'    if the sum the redacted frequencies is also less than or equal to the threshold, then the smallest unredacted frequency is also redacted.
#' @export
redactor2 <- function(n, threshold, x=NULL){

  stopifnot("non-integer values passed to n" = all(n%%1 == 0L))
  stopifnot("threshold must be a scalar" = length(threshold) == 1L)
  stopifnot("non-integer value passed to threshold" = threshold%%1 == 0L)
  stopifnot("n must be non-missing" = all(!is.na(n)))
  stopifnot("n must non-negative" = all(n>=0))

  if(is.null(x)){
    x <- n
  }

  if(!is.null(x)){
    stopifnot("x must be same length as n" = length(n) == length(x))
  }

  n <- as.integer(n)
  leq_threshold <- dplyr::between(n, 1, threshold)
  n_sum <- sum(n)

  # redact if n is less than or equal to redaction threshold
  redact <- leq_threshold

  # also redact next smallest n if sum of redacted n is still less than or equal to threshold
  if((sum(n*leq_threshold) <= threshold) & any(leq_threshold)){
    redact[which.min(dplyr::if_else(leq_threshold, n_sum+1L, n))] = TRUE
  }


  typedNA <- NA
  mode(typedNA) <- typeof(x)

  redacted <- dplyr::if_else(redact, typedNA, x)

  redacted
}



# Functions for summarising then redacting ----

#' Summarise a categorical variable and redact if necessary
#'
#' @param x The vector to summarise and redact.
#' @param threshold The redaction threshold. All values less than or equal to this threshold will be redacted (and possibly more; see the \code{\link{redactor}} function)
#' @param precision The precision of any rounding that is to be applied to frequency values. Defaults to 1 (no rounding).
#' @param .missing_name The string used to replace `NA` categories.
#' @param .redacted_name The string used to replace redacted values.
#' @return A table of redacted frequencies and proportions.
#'
#' @details This function takes a categorical vector (or something that can be coerced to a categorical vector), computes value frequencies and proportions, and redacts according to the rules in \code{\link{redactor}}.
#'
#' @export
redacted_summary_cat <- function(
  x,
  threshold=5L,
  precision=1L,
  .missing_name = "(missing)",
  .redacted_name="redacted"

){

  stopifnot("threshold must be a scalar" = length(threshold) == 1L)
  stopifnot("non-integer value passed to threshold" = threshold%%1 == 0L)
  stopifnot("precision must be a strictly-positive integer" = ((precision>=1) | (precision %% 1)==0))
  stopifnot("x must be a vector" = is.vector(x))


  if (is.logical(x)){
    x <- dplyr::if_else(x, "TRUE", "FALSE")
  }

  dat_freq <- tibble::tibble(
    .level = (forcats::fct_explicit_na(x, na_level=.missing_name)),
  ) %>%
    dplyr::group_by(.level, .drop=FALSE) %>%
    dplyr::tally() %>%
    dplyr::mutate(
      n = as.integer(round(round(n/precision)*precision)), # use as.integer(round()) to avoid floating point error
      prop=n/sum(n),
      n_nonmiss=dplyr::if_else(.level==.missing_name, 0L, n),
      prop_nonmiss = (n_nonmiss/sum(n_nonmiss, na.rm=TRUE)),
    ) %>%
    dplyr::select(-n_nonmiss)

  dat_freq[[.redacted_name]] <- redactor(dat_freq$n, threshold)

  dat_redacted <- dat_freq %>%
    dplyr::mutate(dplyr::across(
      .cols = -tidyselect::all_of(c(".level", .redacted_name)),
      ~{
        dplyr::if_else(dat_freq[[.redacted_name]], .x+NA, .x) # .x+NA rather than NA to ensure correct type
      }
    ))

  dat_redacted
}



#' Categorical by categorical cross-tabulation, with redaction if necessary
#'
#' @param x1 The first categorical variable.
#' @param x2 The second categoical variable.
#' @param threshold The redaction threshold. All values less than or equal to this threshold will be redacted (and possibly more; see the \code{\link{redactor}} function)
#' @param precision The precision of any rounding that is to be applied to frequency values. Defaults to 1 (no rounding).
#' @param .missing_name The string used to replace `NA` categories.
#' @param .redacted_name The string used to replace redacted values.
#' @param .total_name The string used to the label the marginal totals. If NULL, no marginal totals are reported.
#' @return A table of redacted frequencies and proportions, arranged in long-format.
#'
#' @details This function takes two categorical vectors (or vectors that can be coerced to a categorical vectors), performs a cross-tabulation, and redacts according to the rules in \code{\link{redactor}}.
#' proportions are based on x1 totals.
#'
#' @export
redacted_summary_catcat <- function(
  x1,
  x2,
  threshold=5L,
  precision=1L,
  .missing_name = "(missing)",
  .redacted_name="redacted",
  .total_name=NULL
){

  stopifnot("threshold must be a scalar" = length(threshold) == 1L)
  stopifnot("non-integer value passed to threshold" = threshold%%1 == 0L)
  stopifnot("precision must be a strictly-positive integer" = ((precision>=1) | (precision %% 1)==0))
  stopifnot("x1 must be a vector" = is.vector(x1))
  stopifnot("x2 must be a vector" = is.vector(x2))

  if (is.logical(x1)){
    x1 <- if_else(x1, "TRUE", "NO")
  }

  if (is.logical(x2)){
    x2 <- if_else(x2, "TRUE", "NO")
  }

  dat_freq <- tibble::tibble(
    .level1 = (forcats::fct_explicit_na(x1, na_level=.missing_name)),
    .level2 = (forcats::fct_explicit_na(x2, na_level=.missing_name)),
  ) %>%
    dplyr::group_by(.level2, .level1, .drop=FALSE) %>%
    dplyr::tally() %>%
    dplyr::mutate(
      prop = n/sum(n),
      n = as.integer(round(round(n/precision)*precision)),
      n_nonmiss = dplyr::if_else(.level1==.missing_name, 0L, n),
      prop_nonmiss = (n_nonmiss/sum(n_nonmiss, na.rm=TRUE)),
    ) %>%
    dplyr::select(-n_nonmiss)


  dat_freq_redact0 <- dat_freq %>%
    dplyr::group_by(.level1) %>%
    dplyr::mutate(
      .redacted_name1 = redactor(n, threshold),
    ) %>%
    dplyr::group_by(.level2) %>%
    dplyr::mutate(
      .redacted_name2 = redactor(n, threshold)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      {{.redacted_name}} := .redacted_name1 | .redacted_name2
    ) %>%
    dplyr::select(-.redacted_name1, -.redacted_name2)

  #print(dat_freq_redact0)

  dat_redacted <- dat_freq_redact0 %>%
    dplyr::mutate(dplyr::across(
      .cols = -tidyselect::all_of(c(".level1", ".level2", .redacted_name)),
      ~{
        dplyr::if_else(dat_freq_redact0[[.redacted_name]], .x+NA, .x) # .x+NA rather than NA to ensure correct type
      }
    ))


  if(!is.null(.total_name)){
    dat_freq_total_redacted <- redacted_summary_cat(
      x1,
      .missing_name = .missing_name,
      .redacted_name = .redacted_name,
      threshold = threshold
    ) %>%
      dplyr::rename(.level1=.level) %>%
      dplyr::mutate(
        .level2 = factor(.total_name)
      )
    dat_redacted <- dplyr::bind_rows(dat_redacted, dat_freq_total_redacted)
  }

  dat_redacted %>%
    dplyr::select(.level1, .level2, tidyselect::everything()) %>%
    dplyr::arrange(.level2)

}



#' Summarise a numeric vector and redact if necessary
#'
#' @param x The numeric variable.
#' @param threshold The redaction threshold. If the length of `x` is less than or equal to this threshold, then no summary values will be reported.
#' @param .redacted_name The string used to replace redacted values.
#' @return A table of summary statistics for the variable.
#' @details This function takes a numeric vector (or something that can be coerced to one), and summarises it. Summary statistics are redacted according to the rules in \code{\link{redactor}}.
#' @export
redacted_summary_num <- function(x, threshold=5L, .redacted_name="redacted"){

  # TODO add custom_function argument that takes a list of formulas and appends to `summary_fns`.

  stopifnot("threshold must be a scalar" = length(threshold) == 1L)
  stopifnot("non-integer value passed to threshold" = threshold%%1 == 0L)
  stopifnot("x must be a vector" = is.vector(x))

  stats_wide <- tibble::as_tibble_col(
    x, column_name="variable"
  ) %>%
    dplyr::summarise(
      n = length(x),
      n_nonmiss = sum(!is.na(x)),
      prop_nonmiss = sum(!is.na(x))/length(x),
      n_miss = sum(is.na(x)),
      prop_miss = sum(is.na(x))/length(x),

      unique = dplyr::n_distinct(x, na.rm=TRUE),

      mean = mean(x, na.rm=TRUE),
      sd = stats::sd(x, na.rm=TRUE),

      min = min(x, na.rm=TRUE),
      p10 = stats::quantile(x, p=0.1, na.rm=TRUE, type=1),
      p25 = stats::quantile(x, p=0.25, na.rm=TRUE, type=1),
      p50 = stats::quantile(x, p=0.5, na.rm=TRUE, type=1),
      p75 = stats::quantile(x, p=0.75, na.rm=TRUE, type=1),
      p90 = stats::quantile(x, p=0.9, na.rm=TRUE, type=1),
      max = max(x, na.rm=TRUE)

    )

  stats_wide[[.redacted_name]] <- redactor(stats_wide$n, threshold)
  dat_redacted <- stats_wide %>%
    dplyr::mutate(dplyr::across(
      .cols = -tidyselect::all_of(c(.redacted_name)),
      ~{
        dplyr::if_else(stats_wide[[.redacted_name]], .x+NA, .x) # .x+NA rather than NA to ensure correct type
      }
    ))

  dat_redacted
}

# Summarise a numeric vector and redact if necessary


#' Redact a date vector
#'
#' @param x The date variable.
#' @param threshold The redaction threshold. If the length of `x` is less than or equal to this threshold, then no summary values will be reported.
#' @param .redacted_name The string used to replace redacted values.
#' @return A table of summary statistics for the variable.
#'
#' @details This function takes a date vector (or something that can be coerced to one), and summarises it. Summary statistics are redacted according to the rules in \code{\link{redactor}}.
#'
#' @export
redacted_summary_date <- function(x, threshold=5L, .redacted_name="redacted"){

  # TODO add custom_function argument that takes a list of formulas and appends to `summary_fns`.

  stopifnot("input vector is not a date" = inherits(x, c("Date", "POSIXt")))

  stats_wide <- tibble::as_tibble_col(
    x, column_name="variable"
  ) %>%
    dplyr::summarise(
      n = length(x),
      n_nonmiss = sum(!is.na(x)),
      prop_nonmiss = sum(!is.na(x))/length(x),
      n_miss = sum(is.na(x)),
      prop_miss = sum(is.na(x))/length(x),

      unique = dplyr::n_distinct(x, na.rm=TRUE),

      mean = mean(x, na.rm=TRUE),
      sd = stats::sd(x, na.rm=TRUE),

      min = min(x, na.rm=TRUE),
      p10 = stats::quantile(x, p=0.1, na.rm=TRUE, type=1),
      p25 = stats::quantile(x, p=0.25, na.rm=TRUE, type=1),
      p50 = stats::quantile(x, p=0.5, na.rm=TRUE, type=1),
      p75 = stats::quantile(x, p=0.75, na.rm=TRUE, type=1),
      p90 = stats::quantile(x, p=0.9, na.rm=TRUE, type=1),
      max = max(x, na.rm=TRUE)

    )

  stats_wide[[.redacted_name]] <- redactor(stats_wide$n, threshold)
  dat_redacted <- stats_wide %>%
    dplyr::mutate(dplyr::across(
      .cols = -tidyselect::all_of(c(.redacted_name)),
      ~{
        dplyr::if_else(stats_wide[[.redacted_name]], .x+NA, .x) # .x+NA rather than NA to ensure correct type
      }
    ))


  # this step is in case date attribute is lost (eg when only NAs are returned).
  dat_redacted_date <- dat_redacted %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(c("mean","min", "p10", "p25", "p50", "p75", "p90", "max")),
      ~as.Date(.x, "1970-01-01")
    ))

  dat_redacted
}


#' Categorical by numeric cross-tabulation, with redaction if necessary
#'
#' @param variable_cat The categorical vector (or will be coerced to one)
#' @param variable_num The numeric vector
#' @param threshold The redaction threshold. If the length of `x` is less than or equal to this threshold, then no summary values will be reported.
#' @param .missing_name The string used to replace `NA` categories.
#' @param .redacted_name The string used to replace redacted values.
#' @return A table of summary statistics for the numeric variable, stratified by the categorical variable
#'
#' @details This function takes a categorical vector and a numeric vector of the same length, and performs a cross-tabulation. Summary statistics are redacted according to the rules in \code{\link{redactor}}.
#'
#' @export

redacted_summary_catnum <- function(
  variable_cat,
  variable_num,
  threshold=5L,
  .missing_name = "(missing)",
  .redacted_name="redacted"
){

  stats_wide <- tibble::tibble(
    .variable_cat = (forcats::fct_explicit_na(variable_cat, na_level=.missing_name)),
    .variable_num = variable_num
  ) %>%
    dplyr::group_by(.variable_cat) %>%
    dplyr::summarise(.groups="keep",
              n = length(.variable_num),
              n_nonmiss = sum(!is.na(.variable_num)),
              prop_nonmiss = sum(!is.na(.variable_num))/length(.variable_num),
              n_miss = sum(is.na(.variable_num)),
              prop_miss = sum(is.na(.variable_num))/length(.variable_num),

              unique = dplyr::n_distinct(.variable_num, na.rm=TRUE),

              mean = mean(.variable_num, na.rm=TRUE),
              sd = stats::sd(.variable_num, na.rm=TRUE),

              min = min(.variable_num, na.rm=TRUE),
              p10 = stats::quantile(.variable_num, p=0.1, na.rm=TRUE, type=1),
              p25 = stats::quantile(.variable_num, p=0.25, na.rm=TRUE, type=1),
              p50 = stats::quantile(.variable_num, p=0.5, na.rm=TRUE, type=1),
              p75 = stats::quantile(.variable_num, p=0.75, na.rm=TRUE, type=1),
              p90 = stats::quantile(.variable_num, p=0.9, na.rm=TRUE, type=1),
              max = max(.variable_num, na.rm=TRUE),

    )

  stats_wide[[.redacted_name]] <- redactor(stats_wide$n, threshold)

  dat_redacted <- stats_wide %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(
      .cols = -tidyselect::all_of(c(".variable_cat", .redacted_name)),
      ~{
        dplyr::if_else(stats_wide[[.redacted_name]], .x+NA, .x) # .x+NA rather than NA to ensure correct type
      }
    ))

  dat_redacted
}


# functions to convert redacted sumary tables to gt objects ----


#' Convert output of categorical tabulation (redact_summary_cat) to gt object
#'
#' @param x The data.frame produced by `redact_summary_cat`
#' @param var_name The variable name
#' @param pct_decimals Decimal precision for percentages
#' @return A gt object
#'
#' @details This function takes the output of `redact_summary_cat` and converts it to a gt object (as from the `gt` package) for outputting to html/pdf.
#'
#' @export
gt_cat <- function(
  x,
  var_name="",
  pct_decimals = 1
){

  x %>%
    dplyr::select(-c(redacted)) %>%
    gt::gt() %>%
    gt::fmt_percent(
      columns = tidyselect::ends_with(c("prop", "prop_nonmiss")),
      decimals = pct_deminals
    ) %>%
    gt::fmt_missing(
      tidyselect::everything(),
      missing_text="--"
    ) %>%
    gt::tab_spanner(
      label = "non-missing",
      columns = dplyr::vars("n_nonmiss", "prop_nonmiss")
    ) %>%
    gt::cols_label(
      .level=var_name,
      n = "N",
      n_nonmiss = "N",
      prop = "%",
      prop_nonmiss = "%",
    ) %>%
    gt::cols_align(
      align = "left",
      columns = dplyr::vars(.level)
    )
}


#' Convert output of categorical cross-tabulation (redact_summary_cat_cat) to gt object
#'
#' @param x The data.frame produced by `redact_summary_catcat`
#' @param var1_name The name of the first categorical variable
#' @param var2_name The name of the second categorical variable
#' @param title The title of the table
#' @param source_note A footnote
#' @param pct_decimals Decimal precision for percentages
#' @return A gt object
#'
#' @details This function takes the output of `redact_summary_catcat` and converts it to a gt object (as from the `gt` package) for outputting to html/pdf.
#'
#' @export
gt_catcat <- function(
  x,
  var1_name="",
  var2_name="",
  title = NULL,
  source_note = NULL,
  pct_decimals = 1
){

  summary_wide <- x %>%
    dplyr::arrange(.level2) %>%
    tidyr::pivot_wider(
      id_cols=c(.level1),
      values_from=c(n, prop),
      names_from=.level2,
      names_glue="{.level2}__{.value}"
    )

  col_selector <- levels(summary_catcat$.level2)

  old_names <- summary_wide %>% dplyr::select(-.level1) %>% names()

  col_renamer <- old_names %>%
    rlang::set_names(
      . %>%
        #str_replace("__n", str_c("__","N")) %>%
        stringr::str_replace("__prop", stringr::str_c("__","%"))
    )

  gt_table <- summary_wide %>%
    dplyr::rename(!!!col_renamer) %>%
    dplyr::select(.level1, tidyselect::starts_with(paste0(col_selector, "__"))) %>%
    # select step needed because https://github.com/tidyverse/tidyr/issues/839#issuecomment-622073209 -- need
    # use until `gather` option for tab_spanner_delim works
    gt::gt() %>%
    gt::tab_spanner_delim(delim="__", gather=TRUE) %>%
    # gather doesn't work!
    gt::fmt_percent(
      columns = tidyselect::ends_with(c("%")),
      decimals = pct_decimals
    ) %>%
    gt::fmt_missing(tidyselect::everything(),
                missing_text="-"
    ) %>%
    gt::cols_label(
      .level1=var1_name
    ) %>%
    gt::cols_align(
      align = "left",
      columns = dplyr::vars(.level1)
    )

  if(!is.null(title)){
    gt_table <- gt::tab_header(gt_table, title = title)
  }

  if(!is.null(source_note)){
    gt_table <- gt::tab_source_note(gt_table, source_note = source_note)
  }

  gt_table
  ## TODO, add labels, change column headers, redaction label
}



#' Convert output of numeric tabulation (redact_summary_num) to gt object
#'
#' @param x The data.frame produced by `redact_summary_num`
#' @param var_name The variable name
#' @param num_decimals Decimal precision for numbers
#' @param pct_decimals Decimal precision for percentages
#' @return A gt object
#'
#' @details This function takes the output of `redact_summary_num` and converts it to a gt object (as from the `gt` package) for outputting to html/pdf.
#'
#' @export
gt_num <- function(
  x,
  var_name="",
  num_decimals=1,
  pct_decimals=1
){

  x %>%
    dplyr::select(-c(n_miss, prop_miss, redacted)) %>%
    gt::gt() %>%
    gt::fmt_percent(
      columns = tidyselect::ends_with(c("prop_nonmiss", "prop_miss")),
      decimals = pct_deminals
    ) %>%
    gt::fmt_number(
      columns = dplyr::vars("mean", "sd", "min", "p10", "p25", "p50", "p75", "p90", "max"),
      decimals = num_decimals
    ) %>%
    gt::fmt_missing(
      tidyselect::everything(),
      missing_text="--"
    ) %>%
    gt::tab_spanner(
      label = "non-missing",
      columns = dplyr::vars("n_nonmiss", "prop_nonmiss")
    ) %>%
    gt::tab_spanner(
      label = "percentiles",
      columns = dplyr::vars("min", "p10", "p25", "p50", "p75", "p90", "max")
    ) %>%
    gt::cols_label(
      n = "N",
      n_nonmiss = "N",
      prop_nonmiss = "%",
      mean = "mean",
      sd = "SD",
      min = "min",
      unique = "unique values"
    )
}


#' Convert output of categorical-numeric cross-tabulation (redact_summary_catnum) to gt object
#'
#' @param x The data.frame produced by `redact_summary_catnum`
#' @param cat_name The categorical variable name
#' @param num_name The numeric variable name
#' @param num_decimals Decimal precision for numbers
#' @param pct_decimals Decimal precision for percentages
#' @return A gt object
#'
#' @details This function takes the output of `redact_summary_catnum` and converts it to a gt object (as from the `gt` package) for outputting to html/pdf.
#'
#' @export
gt_catnum <- function(
  x,
  cat_name="",
  num_name="",
  num_decimals=1,
  pct_decimals=1
){

  x %>%
    dplyr::select(-c(n_miss, prop_miss, redacted)) %>%
    gt::gt(groupname_col=variable_num) %>%
    gt::fmt_percent(
      columns = tidyselect::ends_with(c("prop_nonmiss", "prop_miss")),
      decimals = pct_deminals
    ) %>%
    gt::fmt_number(
      columns = dplyr::vars("mean", "sd", "min", "p10", "p25", "p50", "p75", "p90", "max"),
      decimals = num_decimals
    ) %>%
    gt::fmt_missing(
      tidyselect::everything(),
      missing_text="--"
    ) %>%
    gt::tab_spanner(
      label = "non-missing",
      columns = dplyr::vars("n_nonmiss", "prop_nonmiss")
    ) %>%
    gt::tab_spanner(
      label = "percentiles",
      columns = dplyr::vars("min", "p10", "p25", "p50", "p75", "p90", "max")
    ) %>%
    gt::cols_label(
      .variable_cat=cat_name,
      n = "N",
      n_nonmiss = "N",
      prop_nonmiss = "%",
      mean = "mean",
      sd = "SD",
      min = "min",
      unique = "unique values"
    ) %>%
    gt::cols_align(
      align = "left",
      columns = dplyr::vars(.variable_cat)
    )
}




# Functions for redacting {gt} objects ----

#' Redact tbl_summary object
#'
#' @param x A tbl_summary object created by the `gt` package.
#' @param threshold The redaction threshold. All values less than or equal to this threshold will be redacted.
#' @param redact_chr The character string used to replace redacted values. Default is "NA".
#' @return A redacted tbl_summary object
#'
#' @details  This function redacts all statistics based on counts less than the threshold (including means, medians, etc)
#'  it also removes potentially disclosive items from the object, namely:
#' - `x$inputs$data` which contains the input data
#' - `x$inputs$meta_data` which contains the raw summary table for the table
#' @export
redact_tblsummary <- function(x, threshold, redact_chr=NA_character_){

  stopifnot("x must be a tbl_summary object" = all(class(x) %in% c("tbl_summary", "gtsummary")))

  raw_stats <- x$meta_data %>%
    dplyr::select(var_label, df_stats) %>%
    tidyr::unnest(df_stats) %>%
    dplyr::mutate(
      redact_n = dplyr::if_else(is.na(n), N_obs, n),
      variable_levels = dplyr::if_else(!is.na(variable_levels), as.character(variable_levels), var_label)
    )

  if(x$inputs$missing %in% c("ifany", "always")){
    missing_stats <- raw_stats %>%
      dplyr::select(by, var_label, variable, N_miss, N_obs, N) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        stat_display = "{N_miss}",
        row_type = "missing",
        redact_n = N_miss,
        variable_levels = x$inputs$missing_text
      ) %>%
      dplyr::filter(N_miss>0 & x$inputs$missing!="always")

    raw_stats <- dplyr::bind_rows(raw_stats, missing_stats)
  }

  name_by_stat <- rlang::set_names(x$df_by$by_col, as.character(x$df_by$by))
  name_stat_by <- rlang::set_names(as.character(x$df_by$by), x$df_by$by_col)

  table_body_long <- x$table_body %>%
    tidyr::pivot_longer(cols=tidyselect::starts_with("stat_"), names_to="by", values_to="display") %>%
    dplyr::mutate(
      by = forcats::fct_recode(by, .fun=!!!name_by_stat)
    )

  redacted_stats <-
    dplyr::left_join(
      raw_stats %>%
        dplyr::select(by, var_label, variable, variable_levels, redact_n),
      table_body_long %>%
        dplyr::filter(!is.na(display)),
      by=c("by", "var_label", "variable", "variable_levels"="label")
    ) %>%
    dplyr::group_by(by, var_label) %>%
    dplyr::mutate(
      display=redactor2(redact_n, threshold, display),
      display = dplyr::if_else(is.na(display), redact_chr, display)
    )

  redacted_body <-
    dplyr::left_join(
      table_body_long %>% dplyr::select(-display),
      redacted_stats %>% dplyr::select(by, variable, var_label, variable_levels, display),
      by=c("by", "variable", "var_label", "label"="variable_levels")
    ) %>%
    dplyr::mutate(
      by = forcats::fct_recode(by, .fun=!!!name_stat_by)
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(variable, var_type, var_class, var_label, row_type, label),
      names_from = by,
      values_from = display
    )

  x$table_body <- redacted_body

  x$inputs$data <- NULL
  x$inputs$meta_data <- NULL
  x$inputs$label <- NULL

  x

}




ceiling_any <- function(x, to=1){
  # round to nearest 100 millionth to avoid floating point errors
  ceiling(plyr::round_any(x/to, 1/100000000))*to
}



# Kaplan-Meier estimates, rounded for disclosure control ----

#' Rounded Kaplan-Meier curves
#'
#' @param data A data frame containing the required survival times
#' @param time Event/censoring time variable, supplied as a character. Must be numeric >0
#' @param event Event indicator variables supplied as a character. Censored (`0`/`FALSE`) or not (`1`/`TRUE`). Must be logical or integer with values zero or one
#' @param strata names of stratification / grouping variables, supplied as a character vector of variable names
#' @param threshold Redact threshold to apply
#' @return A tibble with rounded numbers of at risk, events, censored, and derived survival estimates, by strata
#'
#' @details  This function rounds Kaplan-Meier survival estimates by delaying events times until at least `threshold` events have occurred.
#'
#' @export

round_km <- function(data, time, event, strata=NULL, threshold=6){

  stopifnot("Missing values not allow in `time`" = all(!is.na(data[[time]])))
  stopifnot("Missing values not allow in `event`" = all(!is.na(data[[event]])))

  dat_surv <-
    data %>%
    dplyr::select(tidyselect::all_of(c(time, event, strata))) %>%
    dplyr::rename(time={{ time }}, event = {{ event }}) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(strata))) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      n_events = purrr::map_int(data, ~sum(.x[[event]], na.rm=TRUE)),
      surv_obj = purrr::map(data, ~{
        survfit(Surv(time, event) ~ 1, data = .x, conf.type="log-log")
      }),
      surv_obj_tidy = purrr::map(surv_obj, ~broom::tidy(.x)),
    ) %>%
    dplyr::select(strata, n_events, surv_obj_tidy) %>%
    tidyr::unnest(surv_obj_tidy)


  dat_surv_rounded <-
    dat_surv %>%
    dplyr::mutate(
      # Use ceiling not round. This is slightly biased upwards,
      # but means there's no disclosure risk at the boundaries (0 and 1) where masking would otherwise be threshold/2
      surv = ceiling_any(estimate, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
      surv.ll = ceiling_any(conf.low, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
      surv.ul = ceiling_any(conf.high, 1/floor(max(n.risk, na.rm=TRUE)/(threshold))),
      cml.event = ceiling_any(cumsum(n.event), threshold),
      cml.censor = ceiling_any(cumsum(n.censor), threshold),
      n.event = c(NA, diff(cml.event)),
      n.censor = c(NA, diff(cml.censor)),
      n.risk = ceiling_any(max(n.risk, na.rm=TRUE), threshold) - (cml.event + cml.censor)
    ) %>%
    dplyr::select(tidyselect::all_of(strata), time, surv, surv.ll, surv.ul, n.risk, n.event, n.censor)


  dat_surv_rounded
}
