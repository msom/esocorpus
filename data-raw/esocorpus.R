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

get_isis_unveiled <- function() {
  #'
  #' Gets isis unveiled from the HTML version
  #'
  #' removes page numbers and footnotes
  #'
  page <- read_html("https://www.theosociety.org/pasadena/isis/iu-hp.htm")
  links <- page %>%
    html_elements("a") %>%
    html_attr("href")
  links <- links[grepl("iu[12]-\\d[^0][^b]", links)]

  file <- "data-raw/Blavatsky_Helena Petrovna_1877_Isis Unveiled.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(paste0("https://www.theosociety.org/pasadena/isis/", link))
    page %>%
      html_elements('h5, h3, h4, blockquote, p:not(.link-list-0)') %>%
      html_text() %>%
      str_squish() %>%
      cat(file = file, sep = "\n\n", append = TRUE)
  }

  text <- readChar(file, file.info(file)$size) %>%
    str_replace_all("[\\*†‡§|¶]", "") %>%  # footnote signs
    str_replace_all("[a-z][-]?\\n\\n[a-z]", "")  # paragraphs spanning over pages

  cat(text, file = file)
}

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

# ------------------------------------------------------------------------------
# Process texts
# ------------------------------------------------------------------------------
# Download texts
get_isis_unveiled()

# Read texts
esocorpus = readtext(
  "data-raw/*.[!R]*",
  docvarsfrom = "filenames",
  dvsep = "_",
  docvarnames = c("name", "first.name", "year", "title")
)

# Remove metadata
esocorpus$text <- lapply(esocorpus$text, remove_gutenberg_metadata)
esocorpus$text <- lapply(esocorpus$text, remove_hathitrust_metadata)

# Save
use_data(esocorpus, overwrite = TRUE)
