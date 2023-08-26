# install.packages("stringr")
# install.packages("rvest")

library(stringr)
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
}

# ------------------------------------------------------------------------------
# Download texts
# ------------------------------------------------------------------------------
get_isis_unveiled()
