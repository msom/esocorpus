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

remove_hathitrust_metadata <- function(text) {
  #'
  #' Returns everything after the generated line
  #'
  result <- str_split(text, "Generated on .+ GMT")
  result <- unlist(result)
  if (length(result) > 1) {
    stopifnot(length(result) == 2)
    return(result[2])
  }
  return(result[1])
}

cleanup_blavatsky_isis_unveiled <- function() {
  #'
  #' Cleanup "Isis Unveiled"
  #'
  #' Removes footnote signs and fixes paragraphs spanning over pages.
  #'
  file <- "data-processed/Blavatsky_Helena Petrovna_1877_Isis Unveiled.txt"
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
    "data-processed/Blavatsky_Helena Petrovna_1888_The Secret Doctrine Vol 1.txt",
    "data-processed/Blavatsky_Helena Petrovna_1888_The Secret Doctrine Vol 2.txt",
    "data-processed/Blavatsky_Helena Petrovna_1888_The Secret Doctrine Vol 3.txt"
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
  file <- "data-processed/Westcott_William Wynn_1890_Numbers - Their Occult Powers and Mystic Virtues.txt"
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
  file <- "data-processed/Levi_Eliphas_1856_Transcendental magic, its doctrine and ritual.txt"
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
  file <- "data-processed/Levi_Eliphas_1860_The History of Magic.txt"
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

cleanup_davis_principles <- function() {
  #'
  #' Cleanup "The Principles of Nature"
  #'
  #' Remove headers and collaps paragraphs.
  #' Remove everything before the introduction.
  #' Remove the index and everything afterwards.
  #'
  file <- "data-processed/Davis_Andrew Jackson_1847_The Principles of Nature.txt"

  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("(.{1})\\n*\\d* [ A-Z\\.]*\\n{2}(.{1})", "\\1\n\\2") %>% # remove header (number left)
    str_replace_all("(.{1})\\n*[ A-Z\\.]* \\d*\\n{2}(.{1})", "\\1\n\\2") %>% # remove header (number right)
    str_replace_all("(.{1})-\\n(.{1})", "\\1\\2")  %>% # collapse paragraphs with hyphenation
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2") %>% # collapse paragraphs
    str_replace_all(" {2,}", " ")  # replace multiple spaces with one

  # Remove everything before introduction
  parts <- str_split(text, "a proper understanding of the parts.") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[2]

  # Remove index and everything afterwards
  parts <- str_split(text, "THE END.") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[1]

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
  file <- "data-processed/Cahagnet_Louis Alphonse_1855_The Celestial Telegraph.txt"

  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("(.{1})\\n*\\d* [ A-Z\\.\\^]*\\n{2,}(.{1})", "\\1\n\\2") %>% # remove header (number left)
    str_replace_all("(.{1})\\n*[ A-Z\\.■]* \\d*\\n{2,}(.{1})", "\\1\n\\2") %>% # remove header (number right)
    str_replace_all("(.{1})-\\n(.{1})", "\\1\\2")  %>% # collapse paragraphs with hyphenation
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2") # collapse paragraphs

  # Remove everything before introduction
  parts <- str_split(text, "so sweet a hope !") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[2]

  # Remove ads
  parts <- str_split(text, "CONTENTS Introductory Remarks") %>%
    unlist()
  stopifnot(length(parts) == 2)
  text <- parts[1]

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
  file <- "data-processed/Kerner_Justinus Andreas_1845_The seeress of Prevorst.txt"

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

# ------------------------------------------------------------------------------
# Read raw files and store as text files
# ------------------------------------------------------------------------------
read_raw_files <- function() {
  esocorpus_raw = readtext(
    "data-raw/*.[!R]*",
    docvarsfrom = "filenames",
    dvsep = "_",
    docvarnames = c("name", "first.name", "year", "title")
  )
  for(i in 1:nrow(esocorpus_raw)) {
    cat(
      esocorpus_raw[i, "text"],
      file = paste0(
        "data-processed/",
        str_replace(esocorpus_raw[i, "doc_id"], "pdf", "txt")
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
cleanup_davis_principles()
cleanup_cahagnet_celestial_telegraph()
cleanup_kerner_seeress()

# FIXME: remove_hathitrust_metadata
# Lévi_Éliphas_1868_The Great Secret

# FIXME: remove_gutenberg_metadata
# Lévi_Éliphas_1861_The Key of the Mysteries
# Lévi_Éliphas_1868_The Great Secret

# ------------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------------
esocorpus = readtext(
  "data-processed/*.txt",
  docvarsfrom = "filenames",
  dvsep = "_",
  docvarnames = c("name", "first.name", "year", "title")
)
use_data(esocorpus, overwrite = TRUE)
