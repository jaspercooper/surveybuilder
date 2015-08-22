#' Insert something between questions
#'
#' @param space_reduction Put in a spatial measure to reduce space between questions
#' @param page_break Insert a page break
#' @param line Insert a line
#' @param custom Insert whatever text or functions you like
#' @return An argument or text between questions
#' @export
insert <- function(image_path = NULL,space_reduction = NULL,page_break = FALSE,line = FALSE,custom = NULL,concat = FALSE,...){

  reduce_space <- !is.null(space_reduction)
  insert_custom <- !is.null(custom)
  insert_image <- !is.null(image_path)

  if(sum(reduce_space,insert_custom,page_break,line,insert_image)>1){
    stop("You may only insert one thing at a time.")
  }

  if(insert_image){
    return(insert_image(image_path = image_path,...))
  }

  if(reduce_space){
    insertion <- paste0("\\vspace{",space_reduction,"} \n")
  }

  if(insert_custom){
    insertion <- custom
  }

  if(page_break){
    insertion <- "\\clearpage"
  }

  if(line){
    insertion <- paste0("\\line")
  }

  if(concat)return(cat(insertion))

  return(insertion)

}


#' Insert an image
#'
#' @param image_path The path to the image on the computer (including name and extension of image, eg. image.pdf)
#' @param caption Caption to put on image (optional)
#' @param float_environment Environment to float image in (defaults to "h!")
#' @param image_width Width of image (defaults to "\\textwidth")
#' @param centering Should image be centered?
#' @return An image in the survey
#' @export
insert_image <- function(image_path,caption = NULL,float_environment = "h!",image_width = "\\textwidth",centering = TRUE,caption_above = TRUE,concat = FALSE){

  if(!is.null(caption)){
    caption <- paste0("\\caption{",caption,"}")
  }

  if(!file.exists(image_path)){
    "You have mis-specified the file path to the image."
  }

  if(caption_above){
    caption_above <- caption
    caption_below <- NULL
  }else{
    caption_above <- NULL
    caption_below <- caption
  }

  alignment <- ifelse(centering,"\\centering","")

  image_insert <- paste0(
    "\\begin{figure}[",float_environment,"] \n",
    caption_above, "\n",
    "\\includegraphics[width=",image_width,"]{",image_path,"} \n",
    caption_below, "\n",
    alignment,"\n",
    "\\end{figure} \n"
  )

  if(concat){
    return(cat(image_insert))
  }

  return(image_insert)

}
