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

  annotated_text$doc_id <- gsub("doc", "", annotated_text$doc_id)
  annotated_text$d_s_id <- paste0(annotated_text$doc_id, "_", annotated_text$sentence_id)
  unique_id <- unique(annotated_text$d_s_id)

  cat("Collecting extracted sentences...")

  # extract sentences (orig and lem)
  sentences <- c()
  sentences_lem <- c()
  for (id in unique_id) {
    sentence <- subset(annotated_text, d_s_id == id)[1, 1:4]
    sentences <- rbind(sentences, sentence)
    rownames(sentences) <- NULL
    if (tolower == TRUE) {
      sentences$sentence <- tolower(sentences$sentence)
    }
    if (remove_no == TRUE) {
      sentences$sentence <- stringr::str_remove_all(sentences$sentence, pattern = "[:digit:]") %>%
        stringr::str_squish() %>%
        stringr::str_replace_all(., pattern = "( )([:punct:])", replacement =  "\\2")
    }
    if (remove_punct == TRUE) {
      sentences$sentence <- stringr::str_remove_all(sentences$sentence, pattern = "[:punct:]") %>%
        stringr::str_squish()
    }
    if (lem == TRUE) {
      sentence_lem0 <- subset(annotated_text, d_s_id == id)[,c(1:3, 10)]
      sentence_lem <- cbind(doc_id = sentence_lem0$doc_id[1],
                            paragpraph_id = sentence_lem0$paragraph_id[1],
                            sentence_id = sentence_lem0$sentence_id[1],
                            sentence_lem = paste0(sentence_lem0$lemma, collapse = " ") %>%
                              stringr::str_replace_all(., pattern = "( )([:punct:])", replacement =  "\\2"))
      sentences_lem <- as.data.frame(rbind(sentences_lem, sentence_lem))

      if (tolower == TRUE) {
        sentences_lem$sentence_lem <- tolower(sentences_lem$sentence_lem)
      }
      if (remove_no == TRUE) {
        sentences_lem$sentence_lem <- stringr::str_remove_all(sentences_lem$sentence_lem, pattern = "[:digit:]") %>%
          stringr::str_squish() %>%
          stringr::str_replace_all(., pattern = "( )([:punct:])", replacement =  "\\2")
      }
      if (remove_punct == TRUE) {
        sentences_lem$sentence_lem <- stringr::str_remove_all(sentences_lem$sentence_lem, pattern = "[:punct:]") %>%
          stringr::str_squish()
      }
    }
  }
  if (lem == TRUE) {
    sentences <- cbind(sentences, sentence_lem = sentences_lem$sentence_lem)
  }
  sentences$doc_id <- as.integer(sentences$doc_id)
  class(sentences) <- append(class(sentences), 'sentenceR_df')
  cat("\nDone!")
  return(sentences)
}
