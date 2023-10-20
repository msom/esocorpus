# install.packages("readtext")
# install.packages("stringr")
# install.packages("usethis")
# install.packages("quanteda")
# install.packages("rvest")

library(readtext)
library(stringr)
library(usethis)
library(quanteda)
library(rvest)

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

remove_gutenberg_metadata <- function(text) {
  #'
  #' Returns everything between the start and end markers
  #'
  result <- str_split(text, "\\*\\*\\*.*\\*\\*\\*")
  result <- unlist(result)
  if (length(result) > 1) {
    return(str_trim(result[2]))
  }
  return(result[1])
}

cleanup_blavatsky_isis_unveiled <- function() {
  #'
  #' Cleanup "Isis Unveiled"
  #'
  #' Removes footnote signs and fixes paragraphs spanning over pages.
  #'
  file <- "data-processed/Blavatsky_1877.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("[\\*†‡§|¶]", "") %>%  # footnote signs
    str_replace_all("([a-z][-]?)\\n\\n([a-z])", "\\1 \\2") # paragraphs spanning over pages

  cat(text, file = file)
}

cleanup_blavatsky_secret_doctrine <- function(row) {
  #'
  #' Cleanup "The Secret Doctrine Vol. 1-3"
  #'
  #' Removes footnote signs, underlines (used as emphasis) and collapses
  #' paragraphs to single lines.
  #'
  #' There are only a few hyphenated words, other are double-words such as
  #' "male-female". Since we cannot distinguish between, we assume/keep keep
  #' the latter.
  #'
  files <- c(
    "data-processed/Blavatsky_1888_1.txt",
    "data-processed/Blavatsky_1888_2.txt",
    "data-processed/Blavatsky_1888_3.txt"
  )
  for (file in files) {
    text <- readChar(file, file.info(file)$size) %>%
      remove_gutenberg_metadata() %>%
      str_replace_all("\\(\\d*\\)", "") %>% # footnote signs
      str_replace_all("_", "") %>% # remove underlines for emphasis
      str_replace_all("(.{1})‐\\n(.{1})", "\\1-\\2")  %>% # collapse paragraphs with hyphenation
      str_replace_all("(.{1})\\n(.{1})", "\\1 \\2")  %>% # collapse paragraphs
      str_replace_all("(.{1})\\n    (.{1})", "\\1 \\2") # collapse blockquotes

    cat(text, file = file)
  }
}

cleanup_westcott_numbers <- function() {
  #'
  #' Cleanup "Numbers, Their Occult Power and Mystic Virtues"
  #'
  #' Removes page numbers and fixes paragraphs spanning over pages.
  #'
  file <- "data-processed/Westcott_1890.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("\\[paragraph continues\\]", "") %>%
    str_replace_all("(.{1})\\n\\np\\. \\d{2,}\\n\\n(.{1})", "\\1 \\2") %>% # paragraphs spanning over pages
    str_replace_all("p\\. \\d{2,}.*\\n", "") # left over page number

  cat(text, file = file)
}

cleanup_levi_transcendental_magic <- function() {
  #'
  #' Cleanup "Transcendental magic, its doctrine and ritual"
  #'
  #' Removes headers and collapse paragraphs.
  #' Remove everything before the introduction.
  #' Remove the index and everything afterwards.
  #'
  file <- "data-processed/Levi_1856.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("(.{1})\\n*\\d* [ A-Z]*\\n{2}(.{1})", "\\1\n\\2") %>% # remove header (number left)
    str_replace_all("(.{1})\\n*[ A-Z]* \\d*\\n{2}(.{1})", "\\1\n\\2") %>% # remove header (number right)
    str_replace_all("(.{1})-\\n(.{1})", "\\1\\2")  %>% # collapse paragraphs with hyphenation
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2") # collapse paragraphs

  # Remove everything before introduction
  parts <- str_split(text, "\\nTHE DOCTEINE OF TRANSCENDENT MAGIC\\n") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[2]

  # Remove index and everything afterwards
  parts <- str_split(text, "\\nFINIS.\\n") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[1]

  cat(text, file = file)
}

cleanup_levi_history_of_magic <- function() {
  #'
  #' Cleanup "The History of Magic"
  #'
  #' Collapse paragraphs and removes footnotes.
  #' Remove everything before the introduction.
  #' Remove the index and everything afterwards.
  #'
  file <- "data-processed/Levi_1860.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2") %>% # collapse paragraphs
    str_replace_all("\\[\\d+\\]", "")

  # Remove everything before introduction
  parts <- str_split(text, "XX. Apocalyptic Key: the Seven Seals of St. John\\s*502\\n") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[2]

  # Remove index and everything afterwards
  parts <- str_split(text, "_September 1st, 1859._") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[1]

  cat(text, file = file)
}

cleanup_levi_key_to_the_mysteries <- function() {
  #'
  #' Cleanup "The Key to the Mysteries"
  #'
  #' Collapse paragraphs and removes page number
  #' Remove everything before the the prelimiary considerations
  #' Remove the index and everything afterwards.
  #'
  file <- "data-processed/Levi_1861.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2") %>% # collapse paragraphs
    str_replace_all("\\{\\d+\\}", "") # remove page numbers

  # Remove everything before the prelimiary considerations
  parts <- str_split(text, "miracles and prodigies.") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[2]

  # Remove everything after the epiloge
  parts <- str_split(text, "CONTENTS") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[1]

  cat(text, file = file)
}

cleanup_levi_great_secret <- function() {
  #'
  #' Cleanup "The Great Secret"
  #'
  #' Collapse paragraphs
  #' Remove everything before the first chapter
  #' Remove the biography
  #'
  file <- "data-processed/Levi_1868.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2") %>% # collapse paragraphs
    str_replace_all("([a-z ]{1})\\n+([a-z]{1})", "\\1 \\2") %>% # collapse more paragraphs
    str_replace_all(" {2,}", " ") # remove duplicate spaces

  # Remove everything before the first chapter
  parts <- str_split(text, "‘Studies in Hermetic Tradition’ series.") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[2]

  # Remove the biography
  parts <- str_split(text, "Alphonse Louis Constant") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[1]

  cat(text, file = file)
}

cleanup_papus_tarot <- function() {
  #'
  #' Cleanup "The Tarot of the Bohemians"
  #'
  #' Collapse paragraphs
  #'
  file <- "data-processed/Papus_1892.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("([a-z ]{1})\\n+([a-z]{1})", "\\1 \\2") # collapse paragraphs

  cat(text, file = file)
}


cleanup_davis_principles <- function() {
  #'
  #' Cleanup "The Principles of Nature"
  #'
  #' Duplicates newlines.
  #'
  file <- "data-processed/Davis_1847.txt"

  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("\\n", "\n\n")

  # Remove everything before introduction
  parts <- str_split(text, "The NEW HEAVEN and the NEW EARTH, &C.") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[2]

  cat(text, file = file)
}

cleanup_cahagnet_celestial_telegraph <- function() {
  #'
  #' Cleanup "The Celestial Telegraph"
  #'
  #' Remove headers and collaps paragraphs.
  #' Remove everything before (including) the introduction.
  #' Remove the ads.
  #'
  file <- "data-processed/Cahagnet_1855.txt"

  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("(.{1})-\\n(.{1})", "\\1\\2")  %>% # collapse paragraphs with hyphenation
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2") # collapse paragraphs

  # Remove everything before introduction
  parts <- str_split(text, "so sweet a hope !") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[2]

  cat(text, file = file)
}

cleanup_kerner_seeress <- function() {
  #'
  #' Cleanup "The Seeress of Prevorst.txt"
  #'
  #' Remove headers and collaps paragraphs.
  #' Remove everything before (including) the introduction.
  #' Remove the ads.
  #'
  file <- "data-processed/Kerner_1829.txt"

  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("(.{1})\\n*.*THE SEERESS OF PREVORST.*\\n{2,}(.{1})", "\\1\n\\2") %>% # remove header (number left)
    str_replace_all("(.{1})\\n*[ A-Z]*\\. \\d*[\\?]*\\n{2,}(.{1})", "\\1\n\\2") %>% # remove header (number right)
    str_replace_all("(.{1})-\\n*(.{1})", "\\1\\2")  %>% # collapse paragraphs with hyphenation
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2") %>% # collapse paragraphs
    str_replace_all("(.{1})\\n{2}([a-z]{1})", "\\1 \\2") # collapse some faulty paragraphs

  # Remove everything before introduction
  parts <- str_split(text, "be treated of in this book. ") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[2]

  # Remove index
  parts <- str_split(text, "dT .ai9J8newoJ") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[1]

  cat(text, file = file)
}

cleanup_denis_animal_magnetism <- function() {
  #'
  #' Cleanup "An Introduction to Animal Magnetism"
  #'
  #' Ccollapse paragraphs.
  #' Remove everything before the preface.
  #'
  file <- "data-processed/Denis_1838.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("(.{1})-\\n(.{1})", "\\1\\2")  %>% # collapse paragraphs with hyphenation
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2") # collapse paragraphs

  # Remove everything before introduction
  parts <- str_split(text, "George Denton, Esq., Surgeon, Tottenham ... 387") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[2]

  cat(text, file = file)
}

# ------------------------------------------------------------------------------
# Read raw files and store as text files
# ------------------------------------------------------------------------------
read_raw_files <- function() {
  esocorpus_raw = readtext("data-raw/*.[!R]*")
  for(i in 1:nrow(esocorpus_raw)) {
    cat(
      esocorpus_raw[i, "text"],
      file = paste0(
        "data-processed/",
        str_replace(esocorpus_raw[i, "doc_id"], "pdf|doc", "txt")
      )
    )
  }
}
read_raw_files()

# ------------------------------------------------------------------------------
# Cleanup
# ------------------------------------------------------------------------------
cleanup_blavatsky_isis_unveiled()
cleanup_blavatsky_secret_doctrine()
cleanup_westcott_numbers()
cleanup_levi_transcendental_magic()
cleanup_levi_history_of_magic()
cleanup_levi_key_to_the_mysteries()
cleanup_levi_great_secret()
cleanup_papus_tarot()
cleanup_davis_principles()
cleanup_cahagnet_celestial_telegraph()
cleanup_kerner_seeress()
cleanup_denis_animal_magnetism()

# ------------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------------
esocorpus <- corpus(readtext("data-processed/*.txt"))
metadata <- read.csv("data-processed/metadata.csv")
docvars(esocorpus) <- metadata
for (index in 1:length(esocorpus)) {
  stopifnot(
    startsWith(
      as.character(docid(esocorpus[index,])),
      paste(
        docvars(esocorpus[index,])$name,
        docvars(esocorpus[index,])$year,
        sep = "_"
      )
    )
  )
}
use_data(esocorpus, overwrite = TRUE)
