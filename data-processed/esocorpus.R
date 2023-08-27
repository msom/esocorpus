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

cleanup_isis_unveiled <- function() {
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

cleanup_secret_doctrine <- function(row) {
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

cleanup_numbers <- function() {
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

cleanup_transcendental_magic <- function() {
  #'
  #' Cleanup "Transcendental magic, its doctrine and ritual"
  #'
  #' Removes headers and collapse paragraphs.
  #'
  file <- "data-processed/Levi_Eliphas_1856_Transcendental magic, its doctrine and ritual.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("(.{1})\\n*\\d* [ A-Z]*\\n{2}(.{1})", "\\1\n\\2") %>% # remove header (number left)
    str_replace_all("(.{1})\\n*[ A-Z]* \\d*\\n{2}(.{1})", "\\1\n\\2") %>% # remove header (number right)
    str_replace_all("(.{1})-\\n(.{1})", "\\1\\2")  %>% # collapse paragraphs with hyphenation
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2") # collapse paragraphs

  cat(text, file = file)
}

# ------------------------------------------------------------------------------
# Read raw files and store as text files
# ------------------------------------------------------------------------------
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
rm(esocorpus_raw)

# ------------------------------------------------------------------------------
# Cleanup
# ------------------------------------------------------------------------------
cleanup_isis_unveiled()
cleanup_secret_doctrine()
cleanup_numbers()
cleanup_transcendental_magic()

# FIXME: remove_hathitrust_metadata
# Cahagnet_Louis Alphonse_1855_The Celestial Telegraph
# Davis_Andrew Jackson_1847_The Principles of Nature
# Kerner_Justinus Andreas_1845_The seeress of Prevorst
# Lévi_Éliphas_1868_The Great Secret

# FIXME: remove_gutenberg_metadata
# Lévi_Éliphas_1860_The History of Magic
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
