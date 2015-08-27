#' Randomize survey
#'
#' @param ... A list of all questions or modules, with every possible permutation of every randomized question. Careful attention should be paid to the indexing of this list, as it will be used by the 'question_combinations' object to create full surveys.
#' @param question_combinations A list or matrix of vectors, whose elements index the question/module list, stating the unique combinations questions to be randomized. Each element of the list or column of the matrix should be a vector of integers, where each integer indexes a unique element in the list of questions or modules. Thus, each vector of integers represents a complete survey.
#' @param random_method Define the randomization method. If "complete", then each unique vector in question_combinations is repeated exactly combination_sizes times. If "simple", then each unique vector is generated as a survey with equal probabilities or combination_probs, until the survey is of size N_surveys or sum(combination_sizes). If "asis", randomize_survey simply applies through the question_combinations argument. This method is useful for more complex randomization schemes, where a pre-randomized question_combinations argument is provided.
#' @param combination_probs If "simple" is provided to random_method, column or list element i of question_combinations is chosen with probability combination_probs[i].
#' @param combination_sizes If "simple" is provided to random_method, column or list element i of question_combinations is assigned to combination_sizes[i] units in the sample.
#' @param N_surveys The number of surveys to generate. Should be left blank if combination_sizes is provided.
#' @param ID An optional ID variable to replace the automatically generated one. Should be as long as the number of surveys (i.e. N_surveys or sum(combination_sizes)).
#' @return Outputs all of the uniquely-identified pdfs or .tex files for use in a survey experiment.
#' @export
randomize_survey <- function(...,output_file_path = NULL,question_combinations = NULL,random_method = "simple",combination_probs = NULL,combination_sizes = NULL,N_surveys = NULL,survey_title = NULL,ID = NULL,compile_PDF = TRUE,store_tex = FALSE,page_numbers = TRUE){

  # Generate question list --------------------------------------------------

  question_list <- make_question_list(...)

  questions <- question_list_to_string(question_list)

  # Warnings and errors -----------------------------------------------------

  if(any(grepl(pattern = "~",x = output_file_path))){
    stop("Please remove '~/' and put the full file path into output_file_path")
  }

  if(is.null(question_combinations)){
    warning("You have not provided the combinations of questions to be randomized, so every possible combination of randomized and non-randomized questions will be generated.")
    question_combinations <- get_permutations(question_list = question_list,
                                              numeric_indicators = TRUE)
  }

  if (!any(class(question_combinations) %in% c("list","matrix","data.frame"))) {
    stop(
      "The question_combinations should be in either list, matrix or data.frame form, with unique question combinations as elements of a list, or columns of a matrix or data.frame."
    )
  }

  if (compile_PDF == F & store_tex == FALSE) {
    warning(
      "You are neither compiling PDFs or storing the .tex files, so there will be no output. Please select at least one kind of output."
    )
  }

  if (all(c(!is.null(N_surveys),!is.null(combination_sizes)))) {
    stop(
      "Please provide an argument only to N_surveys or to combination_sizes, not both at the same time."
    )
  }

  if(!any(random_method%in%c("complete","simple","asis"))){
    stop("randomize_survey() only accepts 'simple', 'complete' and 'asis' as random_method.")
  }



  # Transform combination matrix to list ------------------------------------

  if (class(question_combinations) != "list") {
    question_combinations <- lapply(1:ncol(question_combinations),
                                    function(i) {
                                      question_combinations[,i]
                                    })
  }


# Generate survey parameters ----------------------------------------------

  N_combn <- length(question_combinations)

  if (all(c(is.null(N_surveys),is.null(combination_sizes)))) {
    warning(
      "You have not specified N_surveys or combination_sizes, defaulting to one survey per combination."
    )
    N_surveys <- N_combn
  }

  if (!is.null(combination_sizes)) {
    N_surveys <- sum(combination_sizes)
  }

  if (is.null(ID)) {
    ID <- 1:N_surveys
  }


  # Generate surveys --------------------------------------------------------

  if (random_method == "simple") {
    if (is.null(combination_probs)) {
      combination_probs <- rep(1,N_combn) / N_combn
    }

    combn_select <-
      rmultinom(n = N_surveys,size = 1,prob = combination_probs)
    combn_select <-
      apply(combn_select,2,function(draw) {
        which(draw == 1)
      })

    combinations <-
      lapply(combn_select,function(i) {
        question_combinations[[i]]
      })

    surveys <- lapply(1:length(combinations),function(i) {
      combination <- combinations[[i]]
      questions <- unlist(question_list[combination])
      id <- ID[i]
      survey <- make_survey_object(survey_title = survey_title,ID = id,questions = questions)
      return(survey)
    })

  }

  if (random_method == "complete") {
    if (is.null(combination_sizes)) {
      combination_sizes <-
        remaindr(numerator = N_surveys,denominator = N_combn)
    }

    combn_select <- sample(rep(1:N_combn,combination_sizes))

    combinations <-
      lapply(combn_select,function(i) {
        question_combinations[[i]]
      })

    surveys <- lapply(1:length(combinations),function(i) {
      combination <- combinations[[i]]
      questions <- unlist(question_list[combination])
      id <- ID[i]
      survey <- make_survey_object(survey_title = survey_title,ID = id,questions = questions)
      return(survey)
    })


  }

  if(random_method == "asis"){
    surveys <- lapply(1:length(question_combinations),function(i) {
      combination <- question_combinations[[i]]
      questions <- unlist(question_list[combination])
      id <- ID[i]
      survey <- make_survey_object(survey_title = survey_title,ID = id,questions = questions)
      return(survey)
    })
  }

  # Create file path --------------------------------------------------------

  if (is.null(output_file_path)) {
    output_file_path <- paste0(getwd(),"/surveys")
    warning(paste0(
      "No output_file_path specified for surveys, outputting them to ",output_file_path
    ))
  }

  # Generate pdfs -----------------------------------------------------------

  dir.create(output_file_path)

  file.copy(
    from = paste0(path.package("surveybuilder"),"/paperandpencil.sty"),
    to = output_file_path,
    overwrite = FALSE
  )

  for (i in 1:length(surveys)){

    survey_temp <- surveys[[i]]
    tex_name_temp <- paste0(gsub(
      pattern = " ",replacement = "_",x = tolower(survey_temp$survey_title)),"_",
      survey_temp$ID,".tex")

    compile_survey_object(survey_object = survey_temp,
                          output_file_path = output_file_path,
                          compile_PDF = compile_PDF,
                          create_directory = TRUE,
                          keep_tex = store_tex,
                          use_custom_.sty = TRUE,
                          tex_name = tex_name_temp,
                          page_numbers = page_numbers)
  }

  file.remove(paste0(output_file_path,"/paperandpencil.sty"))

}


