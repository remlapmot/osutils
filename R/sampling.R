# Functions for sampling patients based on patient IDs ----


#' Sample n patients (or other observational units) based on patient IDs.
#'
#' @param id An integer patient identifier with the following properties:
#' - consistent between cohort extracts
#' - unique
#' - completely randomly assigned (no correlation with practice ID, age, registration date, etc etc) which should be true as it based on hash of true IDs
#' - strictly greater than zero
#' @param n The number of patients (amongst all those who did not experience the event) to be sampled
#' @return A logical vector indicating whether the patient has been sampled or not
#'
#' @details Result is `TRUE` with probability `max(1,n/length(id))` and `FALSE` with probability `min(0, 1 - n/length(id))`.
#' Patients are selected in ascending order of patient ID until the sampling number is met.
#' Warns (does not fail) if `n` is greater than `length(id)`.
#' @export
sample_random_n <- function(id, n){
  stopifnot("`id` must be integer" = all(id%%1 == 0L))
  stopifnot("`id` must be unique" = dplyr::n_distinct(id)==length(id))
  id <- as.integer(id)
  stopifnot("`n` must be non-negative integer" = n%%1 == 0L & n>=0 & length(n)==1L)

  if (n >length(id)) warning("n=",n, ", which is greater the number of units being sampled (", length(id), ") ")

  dplyr::dense_rank(id) <= n
}




#' Sample a proportion of patients (or other observational units) based on patient IDs
#'
#' @param id An integer patient identifier with the following properties:
#' - consistent between cohort extracts
#' - unique
#' - completely randomly assigned (no correlation with practice ID, age, registration date, etc etc) which should be true as it based on hash of true IDs
#' - strictly greater than zero
#' @param proportion The proportion of patients (amongst all those who did not experience the event) to be sampled
#' @return A logical vector indicating whether the patient has been sampled or not
#'
#' @details Result is `TRUE` with probability `p` and `FALSE` with probability `1-p`.
#' `p` is equal to \cr `ceiling(length(id)*proportion)/length(id)`, which is equal to `proportion` when
#' \cr `length(id)*proportion` is an integer, and slightly higher otherwise.
#' Patients are selected in ascending order of patient ID until the sampling proportion is met.
#' @export
sample_random_prop <- function(id, proportion){
  stopifnot("`id` must be integer" = all(id%%1 == 0L))
  stopifnot("`id` must be unique" = dplyr::n_distinct(id)==length(id))
  id <- as.integer(id)

  stopifnot("`proportion` must be between 0 and 1" = length(proportion==1) & dplyr::between(proportion, 0, 1))

  dplyr::dense_rank(id) <= ceiling(length(id)*proportion)
}




#' Sample patients (or other observational units) based on patient IDs, depending on occurrence of an event or not
#'
#' @param had_outcome A logical indicating if the patient has experienced the outcome or not
#' @param id An integer patient identifier with the following properties:
#' - consistent between cohort extracts
#' - unique
#' - completely randomly assigned (no correlation with practice ID, age, registration date, etc etc) which should be true as it based on hash of true IDs
#' - strictly greater than zero
#' @param n The number of patients (amongst all those who did not experience the event) to be sampled
#' @return A logical vector indicating whether the patient has been sampled or not
#'
#' @details If `had_outcome` is `TRUE` then result is always `TRUE`.
#' If `had_outcome` is `FALSE`, then result is `TRUE` with probability `max(1,n/sum(1-had_outcome))` and `FALSE` with probability \cr `min(0, 1 - n/sum(1-had_outcome))`.
#' Patients are selected in ascending order of patient ID until the sampling number is met.
#' Warns (does not fail) if `n` is greater than `sum(1-had_outcome)`.
#' @export
sample_nonoutcomes_n <- function(had_outcome, id, n){

  stopifnot("`id` must be integer" = all(id%%1 == 0L))
  stopifnot("`id` must be unique" = dplyr::n_distinct(id)==length(id))
  id <- as.integer(id)

  stopifnot("`had_outcome` must be TRUE/FALSE (or 1/0) and not NA" = all(had_outcome %in% c(0L, 1L, TRUE, FALSE)))
  had_outcome <- had_outcome == TRUE

  stopifnot("`n` must be non-negative integer" = n%%1 == 0L & n>=0 & length(n)==1L)
  n <- as.integer(n)

  if (n > sum(1-had_outcome)) warning("n=",n, ", which is greater the number of units being sampled (", sum(1-had_outcome), ") ")

  if(!any(had_outcome)) {dplyr::dense_rank(id) <= n}
  else {  (dplyr::dense_rank(dplyr::if_else(had_outcome, 0L, id)) - 1L) <= n}

}



#' Sample patients (or other observational units) based on patient IDs, depending on occurrence of an event or not
#'
#' @param had_outcome A logical indicating if the patient has experienced the outcome or not
#' @param id An integer patient identifier with the following properties:
#' - consistent between cohort extracts
#' - unique
#' - completely randomly assigned (no correlation with practice ID, age, registration date, etc etc) which should be true as it based on hash of true IDs
#' - strictly greater than zero
#' @param proportion The proportion of patients (amongst all those who did not experience the event) to be sampled
#' @return A logical vector indicating whether the patient has been sampled or not
#'
#' @details If `had_outcome` is `TRUE` then result is always `TRUE`.
#' If `had_outcome` is `FALSE`, then result is `TRUE` with probability `proportion` and `FALSE` with probability `1 - proportion`.
#' Patients are selected in ascending order of patient ID until the sampling proportion is met.
#' @export
sample_nonoutcomes_prop <- function(had_outcome, id, proportion){
  stopifnot("`id` must be integer" = all(id%%1 == 0L))
  stopifnot("`id` must be unique" = dplyr::n_distinct(id)==length(id))
  id <- as.integer(id)

  stopifnot("`had_outcome` must be TRUE/FALSE (or 1/0) and not NA" = all(had_outcome %in% c(0L, 1L, TRUE, FALSE)))
  had_outcome <- had_outcome == TRUE

  stopifnot("`proportion` must be between 0 and 1" = length(proportion==1) & dplyr::between(proportion, 0, 1))

  (dplyr::dense_rank(dplyr::if_else(had_outcome, 0L, id)) - 1L) <= ceiling(sum(!had_outcome)*proportion)
}


#' Derive sampling probabilities
#'
#' @param had_outcome A logical indicating if the patient has experienced the outcome or not
#' @param sampled A logical indicating if a patient was sampled or not
#' @return A numeric vector of the sampling probability
#'
#' @export
sample_weights <- function(had_outcome, sampled){
  # `had_outcome` is a boolean indicating if the subject has experienced the outcome or not
  # `sampled` is a boolean indicating if the patient is to be sampled or not
  dplyr::case_when(
    had_outcome ~ 1,
    !had_outcome & !sampled ~ 0,
    !had_outcome & sampled ~ sum(!had_outcome)/sum((sampled) & !had_outcome),
    TRUE ~ NA_real_
  )
}
