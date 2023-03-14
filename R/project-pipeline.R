# Functions for automating action pipeline from R list to yaml ----


#' Create action object
#'
#' @param name The name of the action. Must be a 1-d character
#' @param run The run command. Must be a 1-d character
#' @param arguments A character vector of arguments to be appended to the run command. Note that all arguments are parsed as strings / characters, so should be converted in-script if needed
#' @param needs A character vector of names of action dependencies
#' @param highly_sensitive A named character vector (or named list) of highly sensitive outputs from the action
#' @param moderately_sensitive A named character vector (or named list) of moderately sensitive outputs from the action
#' @param ... other possible key:value pairs for action types with special parameters
#' @return list
#'
#' @details A named list of length one containing all information needed to define the action and turn it into a yaml chunk.
#' This function can be used a a one-off to create single actions, or used to generate functions that create more specific actions with repeated patterns.
#' All action objects created by this function should be then put together using the `pipeline_list()` function,
#' for instance `pipeline_list(action(...), action(...), action(...), ...)`.
#' If combining 2 or more actions before passing to `pipeline_list()`, use the helper function `c_action()` (similar to `purrr::splice(...)` or `purrr::list_flatten(list(...))`).
#' This ensures that the list of actions has the correct structure. Do not use `list(...)` or similar!
#' @export
pipeline_action <- function(
    name,
    run,
    arguments=NULL,
    needs=NULL,
    highly_sensitive=NULL,
    moderately_sensitive=NULL,
    ... # other arguments / options for special action types
){
  outputs <- list(
    highly_sensitive = as.list(highly_sensitive),
    moderately_sensitive = as.list(moderately_sensitive)
  )
  outputs[sapply(outputs, function(x) {length(x)==0})] <- NULL # if no output specified then remove empty list element

  if(!is.null(names(arguments))) {
    arguments <- paste(paste0("--", names(arguments)), arguments, collapse=" ")
  }

  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    needs = unname(needs),
    outputs = outputs,
    ... = ...
  )
  action[sapply(action, is.null)] <- NULL # if no key:value specified then remove empty list element

  action_list <- list(action)
  names(action_list) <- name

  action_list
}


#' Create comment object
#'
#' @param ... a collection of actions and lists of actions.
#' @return A list of actions.
#'
#' @details Use this to combine action objects before passing to `project_list()`.
#' This ensures that the list of actions has the correct structure. Do not use `list(...)` or similar!
#' @export
c_action <- function(...){
  purrr::list_flatten(unname(tibble::lst(...))) #use of lst ensures trailing commas to not cause problems
}

#' Create comment object
#'
#' @param ... character or -character-convertible objects
#' @return A list
#'
#' @details key:value list element that will be converted to a comment block in yaml when `project_list_to_yaml()` is run.
#' Each comment will be prefixed by "## " and suffixed by " ##".
#' These comments are first converted to `'': '## your comment here ##'` in yaml, and then tidied up to `## your comment here ##` before saving.
#' @export
pipeline_comment <- function(...){
  list_comments <- unname(tibble::lst(...))
  comments <- lapply(list_comments, function(x) paste0("## ", x, " ##"))
  comments
}

#' Create entire pipeline list
#'
#' @param ... all actions and comments that go into the entire project pipeline.
#' These can be provided as a mixture of single actions (from `pipeline_action()` function) or as lists of actions (from `c_action()` function.)
#' @param .version version of opensafely to use
#' @param .population_size size of dummy data expectations
#' @return A list
#'
#' @details This function is used to put all actions together in the entire project list,
#' as well as specifying the project frontmatter (version and expectations).
#' @export
pipeline_list <- function(..., .version="3.0", .population_size=1000L){
  list(
    version = .version,
    expectations= list(population_size=.population_size),
    actions = c_action(...)
  )
}

#' Convert list to yaml and save
#'
#' @param project_list list object containing all actions (created using action function) and comment-actions (created using comment_action function) and front-matter.
#' @param filepath file path and name where yaml file should be saved. If not provided, then prints to console!
#'
#' @details Convert list to yaml string and then prints or saves the results. This also does some reformatting of comment blocks, whitespace, etc.
#' @export
project_list_to_yaml <- function(project_list, filepath=NULL){

  yaml_string <- yaml::as.yaml(project_list, indent=2) %>%
    # convert comment actions to comments
    stringr::str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    stringr::str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    stringr::str_replace_all("\\#\\#\\'\\\n", "\n") %>%
    # add one blank line before level 1 and level 2 keys
    stringr::str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
    stringr::str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1")
  if(is.null(filepath)){
    writeLines(yaml_string)
  } else {
    if(!(fs::path_ext(filepath) %in% c("yaml", "yml"))) {warning("The file extension is not 'yaml' or 'yml'")}
    writeLines(yaml_string, con=filepath)
  }
}

#' Put action names in a txt file ----
#'
#' @param action_list list of project actions
#' @param filepath file path and name where .txt file should be saved. If not provided, then prints to console!
#'
#' @details grab all action names and send to a txt file.  "action_list" should be the "actions" list entry in the "project_list" object (i.e., `project_list$actions`)
#' @export
action_names_to_txt <- function(action_list, filepath=NULL){
  action_names <-
    names(action_list) %>%
    {.[.!=""]} %>%
    paste(collapse="\n")

  if(is.null(filepath)){
      writeLines(action_names)
  } else {
    if(!(fs::path_ext(filepath) %in% c("txt"))) {warning("The file extension is not 'txt'")}
    writeLines(action_names, con=filepath)
  }
}

