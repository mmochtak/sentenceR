# sentenceR <img src="/man/logo.png" style="max-width:100%;" height="139" align="right">
*R package for (almost) language-agnostic sentence tokenization*

**sentenceR** is a language-agnostic utility designed for sentence tokenization of raw text. Using the UDPipe POS tagging pipeline, the package automatically extracts sentences with their appropriate indexes (hence the “crowbar” logo as a reference to extraction). The package works with any of the 100+ language models natively provided by UDPipe package (see https://github.com/bnosac/udpipe).

## Overview
The package is intended for use especially with non-English languages that are under-resourced in terms of standardized tools. The approach is not particularly fast as the pipeline relies on full POS tagging done by UDPipe but it provides a reliable option for programmers working on small corpora of various origins (typically social scientists) who need a simple pre-processing tool for extracting sentence-level tokens and their higher n-grams. For the convenience, the main function provides several control arguments for cleaning the raw text as well as extracting its lemmatized form for further processing.

## Installation Instruction
Install the package from the GitHub repository:
```
devtools::install_github('mmochtak/sentenceR')
```
## Version
0.0.2

## Usage
The package contains three general functions: *get_sentences*; *sent_ngrams*; *sent_ngrams_lem*

-	*get_sentences* is a general function for extraction sentences from raw text. The main input is a string vector. The only other required argument is the language model (here, for simplicity, “english”). Additionally, if needed the output can be further cleaned off of numbers (add argument *remove_no = TRUE*) and punctuation (add argument *remove_punct = TRUE*), all characters can be transformed to lower cases (add argument *tolower = TRUE*), as well as the outcome sentence can be accompanied by its lemmatized version (add argument *lem = TRUE*; see example below). By default, only 1 core is used for the processing. If more cores are available, parallelization can be initialized with *n_cores* argument (e.g. *n_cores = 4* will use four cores).

```
library(sentenceR)
sample_text <- c("This is sentence number one. This is sentence number two. This is sentence number three.", "This is sentence number four. This is sentence number five. This is sentence number six.")

get_sentences(text = sample_text, language = "english", lem = TRUE, verbose = TRUE)

  doc_id paragraph_id sentence_id                       sentence                   sentence_lem
1      1            1           1   This is sentence number one.   this be sentence number one.
2      1            1           2   This is sentence number two.   this be sentence number two.
3      1            1           3 This is sentence number three. this be sentence number three.
4      2            1           1  This is sentence number four.  this be sentence number four.
5      2            1           2  This is sentence number five.  this be sentence number five.
6      2            1           3   This is sentence number six.   this be sentence number six.
```

-	*sent_ngrams* is a simple function that takes the result of *get_sentences* (a data frame) and creates higher sentence n-grams based on a specified *n*.

```
library(sentenceR)
sample_text <- c("This is sentence number one. This is sentence number two. This is sentence number three.", "This is sentence number four. This is sentence number five. This is sentence number six.")

result <- get_sentences(text = sample_text, language = "english", lem = TRUE, verbose = TRUE)
sent_ngrams(sentences = result, n = 2)

  doc_id ngram_id                                                       ngram
1      1        1   This is sentence number one. This is sentence number two.
2      1        2 This is sentence number two. This is sentence number three.
3      2        1 This is sentence number four. This is sentence number five.
4      2        2  This is sentence number five. This is sentence number six.
```

-	*sent_ngrams_lem* is an equivalent to *sent_ngrams* but instead of regular sentences their lemmatized version are taken as an input (column sentence_lem must exist in the get_sentences data frame).

```
library(sentenceR)
sample_text <- c("This is sentence number one. This is sentence number two. This is sentence number three.", "This is sentence number four. This is sentence number five. This is sentence number six.")

result <- get_sentences(text = sample_text, language = "english", lem = TRUE, verbose = TRUE)
sent_ngrams_lem(sentences = result, n = 2)

  doc_id ngram_id                                                       ngram
1      1        1   this be sentence number one. this be sentence number two.
2      1        2 this be sentence number two. this be sentence number three.
3      2        1 this be sentence number four. this be sentence number five.
4      2        2  this be sentence number five. this be sentence number six.
```

## Final Remarks
The package has been developed as a practice for me as I wanted to learn how to create packages to better organize my code. However, it might be useful for anybody working on NLP tasks requiring sentence tokenization and lemmatization in languages other than English. I know the approach is not the most efficient in terms of speed and accuracy but it is very versatile when it comes to languages that can be processed out of the box (for a slightly extended tutorial see [my blog](https://mmochtak.github.io/sentenceR/)). Regarding accuracy, it is important to stress that sentence tokenization is based on accuracy of language models and may differ among models. Although I’ve never planned to submit it to CRAN, I am planning to maintain it as long as it is still useful via GitHub. Feel free to contact me if standard GitHub channels are not suitable for you via my [personal website](https://mochtak.com/). If used, please cite it as:

> Mochtak, Michal (2021): *sentenceR*: Language-Agnostic Sentence Tokenization for Low-Resourced Languages. URL: https://github.com/mmochtak/sentenceR/. DOI: 10.13140/RG.2.2.12134.65608
