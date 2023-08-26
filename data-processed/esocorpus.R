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
  #' Cleanup Isis unveiled
  #'
  file <- "data-processed/Blavatsky_Helena Petrovna_1877_Isis Unveiled.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("[\\*†‡§|¶]", "") %>%  # footnote signs
    str_replace_all("([a-z][-]?)\\n\\n([a-z])", "\\1 \\2") # paragraphs spanning over pages

  cat(text, file = file)
}

cleanup_secret_doctrine <- function(row) {
  #'
  #' Cleanup Secret Doctrine Vol. 1-3
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

cleanup_key_to_theosophy <- function() {
  #'
  #' Cleanup Secret Doctrine Vol. 1-3
  #'
  #' There seems to be no hyphenated words, only double-words such as
  #' "self-denial".
  #'
  #' There are some paragraphs spanning over multiple new lines, we fix only
  #' the ones continuing with lower case letters.

  file <- "data-processed/Blavatsky_Helena Petrovna_1889_The Key to Theosophy.txt"
  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("\\(\\d*\\)", "") %>% # footnote signs
    str_replace_all("(.{1})‐\\n(.{1})", "\\1-\\2")  %>% # collapse paragraphs with hyphenation
    str_replace_all("(.{1})\\n(.{1})", "\\1 \\2")  %>% # collapse paragraphs
    str_replace_all("(.{1})\\n    (.{1})", "\\1 \\2") %>% # collapse blockquotes
    str_replace_all("([a-z][-]?)\\n\\n([a-z])", "\\1 \\2") # paragraphs spanning over pages

  cat(text, file=file)
}

# ------------------------------------------------------------------------------
# Read files and store as text files
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
cleanup_key_to_theosophy()

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
