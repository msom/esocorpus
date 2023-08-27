# install.packages("stringr")
# install.packages("rvest")

library(stringr)
library(rvest)

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

fetch_isis_unveiled <- function() {
  #'
  #' Gets "Isis Unveiled" from the HTML version
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

fetch_key_to_theosophy <- function() {
  #'
  #' Gets "Key to Theosophy" from the HTML version
  #'
  #' Does not include the preface.
  #'
  page <- read_html("https://www.theosociety.org/pasadena/key/key-hp.htm")
  links <- page %>%
    html_elements("a") %>%
    html_attr("href")
  links <- links[grepl("key-[^g]", links)]
  links <- links[!duplicated(links)]

  file <- "data-raw/Blavatsky_Helena Petrovna_1889_The Key to Theosophy.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(paste0("https://www.theosociety.org/pasadena/key/", link))
    page %>%
      html_elements('h5, h3, h4, blockquote, p:not(.link-list-0)') %>%
      html_text() %>%
      str_squish() %>%
      cat(file = file, sep = "\n\n", append = TRUE)
  }
}

fetch_voice_of_silence <- function() {
  #'
  #' Gets "The Voice of the Silence" from the HTML version.
  #'
  #' Does not include the preface.
  #'
  page <- read_html("https://www.theosociety.org/pasadena/voice/voice.htm")
  links <- page %>%
    html_elements("a") %>%
    html_attr("href")
  links <- links[grepl("voice", links)]

  file <- "data-raw/Blavatsky_Helena Petrovna_1889_The Voice of the Silence.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(paste0("https://www.theosociety.org/pasadena/voice/", link))
    page %>%
      html_elements('h5, h3, h4, blockquote, p:not(.link-list-0)') %>%
      html_text() %>%
      str_squish() %>%
      cat(file = file, sep = "\n\n", append = TRUE)
  }
}

fetch_numbers <- function() {
    #'
    #' Gets "Numbers, Their Occult Power and Mystic Virtues".
    #'
    #' Does not include the prefaces.
    #'
    page <- read_html("https://sacred-texts.com/eso/nop/index.htm")
    links <- page %>%
      html_elements("a") %>%
      html_attr("href")
    links <- links[grepl("nop0[4-9]|nop[12]\\d", links)]

    file <- "data-raw/Westcott_William Wynn_1890_Numbers - Their Occult Powers and Mystic Virtues.txt"
    cat("", file = file)
    for (link in links) {
      page <- read_html(paste0("https://sacred-texts.com/eso/nop/", link))
      page %>%
        html_elements('h1, h2, h3, h4, p:not([align]), div:not(.filenav)') %>%
        html_text() %>%
        str_squish() %>%
        cat(file = file, sep = "\n\n", append = TRUE)
    }
}

# ------------------------------------------------------------------------------
# Download texts
# ------------------------------------------------------------------------------
fetch_isis_unveiled()
fetch_key_to_theosophy()
fetch_voice_of_silence()
fetch_numbers()
