#' Create a question.
#'
#' @param question_prompt The main text of the question.
#' @param type The type of question to create ("Open", etc.)
#' @return A survey question
#' @export
make_question <-
  function(question_prompt,type = "open",answer_categories = NULL,comment = NULL){
    question <- "The question text."
    # This will function as a wrapper for all of the other make_question functions
    return(print(question))
  }


#' Create an open question.
#'
#' @param question_prompt The main text of the question.
#' @param concat Logical argument: should the text be concatenated and printed using cat(), i.e. for sinking directly to .tex files.
#' @return An open survey question.
#' @examples make_open_question("How are you?")
#' @export
make_open_question <- function(question_prompt,concat = F){
  s_question <- paste0("\\question{{",question_prompt,"}")
  # Add in here options for comments, etc.
  question <- paste0(s_question,"}")
  if(concat){
    return(cat(question))
  }else{
    return(print(question))
  }
}




