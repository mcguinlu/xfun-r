#' Format URL, splitting to fit in editing window
#'
#' @param length What is the maximum length of each chunk of the path. Default
#'   is 80
#'
#' @export

format_url <- function(length = 80) {

  context <- rstudioapi::getSourceEditorContext()

  info <- get_info(context)

  start <- as.numeric(info$range[[1]][2])

  # = two quotes, one comma, and two spaces for indenting
  length <- length - start - 3

  spaces <- paste0(rep(" ",start+1), collapse = "")
  spaces_min <- paste0(rep(" ",start-1), collapse = "")

  collapse_term <- paste0("\",\n",spaces,"\"")

  # Reformat the text
  if (nchar(info[["text"]]) > 0) {
    text <- info[["text"]]
  } else {
    text <- paste0("\"",clipr::read_clip(),"\"")
  }

  path_text <- split_given_length(text = text,
                             length = length)

  path_text <- paste0("paste0(\n",
                      spaces,
                 paste(path_text, collapse = collapse_term),
                 "\n",
                 spaces_min,
                 ")")

  # Update highlighted code
  rstudioapi::modifyRange(info[["range"]], path_text, info[["id"]])


}

#' Split
#'
#' @param text Text to be split
#' @param length What is the maximum length of each chunk of the path. Default
#'   is 80
#' @export
split_given_length <- function(text,length = 80) {

  num.chars <- nchar(text)

  # the indices where each substr will start
  starts <- seq(1, num.chars, by=length)

  # chop it up
  text <- sapply(starts, function(ii) {
    substr(text, ii, ii+length-1)
  })

  return(text)

}

#' Get information on the highlighted path from the editor
#'
#' @param context Context of the text
#'
#' @keywords internal

get_info <- function(context) {

  info <- list(range = rstudioapi::primary_selection(context)[["range"]],
               text = rstudioapi::primary_selection(context)[["text"]],
               path = context[["path"]],
               id = context[["id"]])

  return(info)
}
