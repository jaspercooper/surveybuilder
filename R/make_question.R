#' Create a question.
#'
#' @param question_prompt The main text of the question.
#' @param type The type of question to create ("Open", etc.)
#' @return A survey question
#' @export
make_question <-
  function(question_type,...){
    if(missing(question_type)){
      stop("Please choose the type of question you want to make: 'open','horizontal','vertical', 'prompt', or 'numeric'. See make_open_question(), make_horizontal_question(), make_vertical_question(), make_numeric_question(), and make_prompt() for more information on the functionality of each question type.")
    }

    if(!question_type%in%c("open","horizontal","vertical","numeric","prompt")){
      stop("make_question() can only make questions of type 'open','horizontal','vertical', 'prompt', or 'numeric'.")
    }

    if(question_type=="open"){
      question <- make_open_question(...)
    }

    if(question_type=="horizontal"){
      question <- make_horizontal_question(...)
    }

    if(question_type=="vertical"){
      question <- make_vertical_question(...)
    }

    if(question_type=="numeric"){
      question <- make_numeric_question(...)
    }

    if(question_type=="prompt"){
      question <- make_prompt(...)
    }

    return(question)
  }


#' Create an open question.
#'
#' @param question_prompt The main text of the question.
#' @param concat Logical argument: should the text be concatenated and printed using cat(), i.e. for sinking directly to .tex files.
#' @param N_lines How many lines to print for the response
#' @param N_dot_lines How many dotted lines to print for the response
#' @return An open survey question.
#' @examples make_open_question("How are you?")
#' @export
make_open_question <- function(question_prompt,comment = NULL,concat = F,N_lines = NULL, N_dot_lines = NULL){

  if(!is.null(N_lines)&!is.null(N_dot_lines)){
    stop("You should supply an argument to N_lines or to N_dot_lines, not to both.")
  }

  s_question <- paste0("\\question{{",question_prompt,"}")
  if(!is.null(comment)){
    s_question <- paste0(s_question," \\\\",comment)
  }
  question <- paste0(s_question,"}")

  if(!is.null(N_lines)){
    number <- numbers2words(N_lines)
    n_lines <- paste0("\\open",number)
    question <- paste0(question," \n ",n_lines)
  }

  if(!is.null(N_dot_lines)){
    number <- numbers2words(N_dot_lines)
    n_dot_lines <- paste0("\\opendots",number)
    question <- paste0(question," \n ",n_dot_lines)
  }


  if(concat){
    return(cat(question))
  }else{
    return(question)
  }
}


#' Create a horizontal multiple choice question
#'
#' @param question_prompt The main text of the question.
#' @param concat Logical argument: should the text be concatenated and printed using cat(), i.e. for sinking directly to .tex files.
#' @return An open survey question.
#' @examples make_open_question("How are you?")
#' @export
make_horizontal_question <- function(question_prompt = NULL,comment = NULL,upper_labels = NULL,lower_labels = NULL,concat = F){

  if(all(sapply(c(lower_labels,upper_labels),is.null))){
    stop("You must specify either lower or upper labels.")
  }

  if(!is.null(upper_labels)&!is.null(lower_labels)){
    if(length(upper_labels)!=length(lower_labels)){
      stop("upper_labels and lower_labels should be vectors of equal length. For blank labels, please use NA.")
    }
  }

  if(is.null(lower_labels)){
    lower_labels <- rep("",length(upper_labels))
  }
  if(is.null(upper_labels)){
    upper_labels <- rep("",length(lower_labels))
  }

  upper_labels[is.na(upper_labels)] <- ""
  lower_labels[is.na(lower_labels)] <- ""

  if(!is.null(question_prompt)){
    s_question <- paste0("\\question{{",question_prompt,"}")
    if(!is.null(comment)){
      s_question <- paste0(s_question," \\\\",comment)
    }
    question <- paste0(s_question,"}")
  }else{question <- NULL}

  number <- numbers2words(length(upper_labels))

  horizontal_s <- paste0("\\horizontal",number)

  lower_s <- paste0("\\down",number,"{",paste(lower_labels,collapse = "}{"),"}")
  upper_s <- paste0("\\up",number,"{",paste(upper_labels,collapse = "}{"),"}")

  horizontal <- paste0(horizontal_s,"{",lower_s,"} \n {",upper_s,"}")

  full_question <- paste0(question, " \n ",horizontal)

  if(concat){
    return(cat(full_question))
  }else{
    return(full_question)
  }
}


#' Create a vertical multiple choice question
#'
#' @param question_prompt The main text of the question.
#' @param concat Logical argument: should the text be concatenated and printed using cat(), i.e. for sinking directly to .tex files.
#' @return An open survey question.
#' @examples make_open_question("How are you?")
#' @export
make_vertical_question <- function(question_prompt = NULL,comment = NULL,labels = NULL,concat = F){

  labels[is.na(labels)] <- ""

  if(!is.null(question_prompt)){
    s_question <- paste0("\\question{{",question_prompt,"}")
    if(!is.null(comment)){
      s_question <- paste0(s_question," \\\\",comment)
    }
    question <- paste0(s_question,"}")
  }else{question <- NULL}


  vertical_s <- "\\begin{longanswersC} \n"
  vertical_e <- "\\end{longanswersC} \n"

  categories <- ""
  for(i in 1:length(labels)){
    categories <- paste0(categories,"\\item ",labels[i]," \n ")
  }

  full_question <- paste0(question," \n ", vertical_s, " \n ",categories," \n ",vertical_e)

  if(concat){
    return(cat(full_question))
  }else{
    return(full_question)
  }
}


#' Create a numerical box question
#'
#' @param question_prompt The main text of the question.
#' @param concat Logical argument: should the text be concatenated and printed using cat(), i.e. for sinking directly to .tex files.
#' @return An open survey question.
#' @examples make_open_question("How are you?")
#' @export
make_numeric_question <-
  function(question_prompt = NULL,comment = NULL,labels = NULL,N_boxes = 2,cm_from_left = 8,concat = F) {
    if (!is.null(question_prompt)) {
      s_question <- paste0("\\question{{",question_prompt,"}")
      if (!is.null(comment)) {
        s_question <- paste0(s_question," \\\\ ",comment)
      }
      question <- paste0(s_question,"}")
    }else{
      question <- NULL
    }

    if (length(N_boxes) != length(labels)) {
      stop("N_boxes and labels should be vectors of the same length.")
    }

    responses <- ""

    for (i in 1:length(labels)) {
      responses <- paste0(responses," $ ",paste0(rep("\\bbox",
                                                    N_boxes[i]),collapse = "")," $ ",labels[i]," ")
    }
    responses <- paste0(" $\\hspace{",cm_from_left,"cm}$ ",responses," $\\hfill$ ")

    full_question <- paste0(question, " \n ",responses)

    if (concat) {
      return(cat(full_question))
    }else{
      return(full_question)
    }
  }


#' Create a prompt
#'
#' @param prompt The non-question prompt or LaTeX formatting
#' @return A prompt.
#' @export
make_prompt <-
  function(prompt = NULL,concat = F) {
    return(prompt)
  }







