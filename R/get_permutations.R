#' Get all the unique combinations of questions from a list of survey questions
#'
#' @param ... A list of questions or survey modules
#' @param type The type of question to create ("Open", etc.)
#' @return A survey question
#' @export
get_permutations <- function(question_list,numeric_indicators = TRUE){

  random_scheme <- get_randomization_scheme(question_list)
  name_structure <- get_names(question_list,random_scheme)
  question_perms <- t(expand.grid(name_structure))
  rownames(question_perms) <- NULL

  if(numeric_indicators){
    question_names <- get_names(question_list,random_scheme,as_list = F)
    question_perms <- apply(question_perms,2,match,table = question_names)
  }

  return(question_perms)

}
