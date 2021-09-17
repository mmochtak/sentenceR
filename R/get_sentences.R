#' Function extracting sentences form raw text
#'
#' This function allows tokenizing text on the level of sentences.
#' @param text Vector of strings that is going to be tokenized.
#' @param language Language model that is used for tokenization. See language models at https://github.com/bnosac/udpipe.
#' @param lem Logical parameter for extracting also lemmatized version of a sentence. Default is FALSE.
#' @param remove_no Logical parameter for removing numbers. Default is FALSE.
#' @param remove_punct Logical parameter for removing punctuation. Default is FALSE.
#' @param tolower Logical parameter for transforming strings to lower case. Default is FALSE.
#' @param verbose Logical parameter for displaying extended information on processed data. Works only with processing on one core. Default is FALSE.
#' @param n_cores Numeric parameter for number of cores to be used for processing. Default is 1 core.
#' @keywords tokenization sentence language agnostic
#' @export
#' @examples
#' get_sentences()

get_sentences <- function (text, language, lem = FALSE, remove_no = FALSE, remove_punct = FALSE,
                           tolower = FALSE, verbose = FALSE, n_cores = 1) {
  if (isFALSE(isTRUE(class(text) == "character"))) stop("Text input must be a character vector.")
  if (isFALSE(class(language) == "character")) stop("Language input must be a string referring to a UDPipe langauge model; see list of models at https://github.com/bnosac/udpipe, section 'Pre-trained models'.")
  if (isFALSE(class(lem) == "logical")) stop("'lem' input must be of class 'logical'.")
  if (isFALSE(class(remove_no) == "logical")) stop("'remove_no' input must be of class 'logical'.")
  if (isFALSE(class(remove_punct) == "logical")) stop("'remove_punct' input must be of class 'logical'.")
  if (isFALSE(class(tolower) == "logical")) stop("'tolower' input must be of class 'logical'.")
  if (isFALSE(class(verbose) == "logical")) stop("'verbose' input must be of class 'logical'.")
  if (isFALSE(class(n_cores) == "numeric")) stop("'n_cores' input must be of class 'numeric'.")

  # text annotation
  model_exist <- stringr::str_detect(list.files("./"), pattern = paste0("^", language, "-")) %>%
    which(isTRUE(.))
  if (verbose == TRUE) {
    verbose <- 1L
  }
  if (length(model_exist) == 0) {
    model <- udpipe::udpipe_download_model(language = language)
    annotated_text <- udpipe::udpipe(udpipe::udpipe_load_model(file = model$file_model),
                                     x = text, trace = verbose, parallel.cores = n_cores)
  } else {
    annotated_text <- udpipe::udpipe(udpipe::udpipe_load_model(list.files("./")[model_exist]),
                                     x = text, trace = verbose, parallel.cores = n_cores)
  }

  annotated_text$doc_id <- gsub("doc", "", annotated_text$doc_id) %>% as.numeric()

  cat("Collecting extracted sentences...")

  # extract sentences (orig and lem)
  sentences <- annotated_text %>%
                dplyr::group_by(doc_id, paragraph_id, sentence_id) %>%
                dplyr::summarise(sentence = sentence[1], sentence_lem = paste(lemma, collapse = " "))

  # fix lem punctuation
  sentences$sentence_lem <- sentences$sentence_lem %>%
                            stringr::str_replace_all(pattern = "( )([:punct:] )", replacement = "\\2") %>%
                            stringr::str_replace_all(pattern = "( )([:punct:]$)", replacement = "\\2") %>%
                            stringr::str_replace_all(pattern = "\\( ", replacement = " \\(")

  # apply user-defined settings
  if (tolower == TRUE) {
    sentences$sentence <- tolower(sentences$sentence)
    sentences$sentence_lem <- tolower(sentences$sentence_lem)
  }
  if (remove_no == TRUE) {
    sentences$sentence <- stringr::str_remove_all(sentences$sentence, pattern = "[:digit:]") %>%
                          stringr::str_squish() %>%
                          stringr::str_replace_all(., pattern = "( )([:punct:])", replacement =  "\\2")

    sentences$sentence_lem <- stringr::str_remove_all(sentences$sentence_lem, pattern = "[:digit:]") %>%
                              stringr::str_squish() %>%
                              stringr::str_replace_all(., pattern = "( )([:punct:])", replacement =  "\\2")
  }
  if (remove_punct == TRUE) {
    sentences$sentence <- stringr::str_remove_all(sentences$sentence, pattern = "[:punct:]") %>%
                          stringr::str_squish()
    sentences$sentence_lem <- stringr::str_remove_all(sentences$sentence_lem, pattern = "[:punct:]") %>%
                              stringr::str_squish()
  }

  if (lem != TRUE) {
    sentences <- sentences[,-5]
  }

  cat("\nDone!")
  return(sentences)
}
