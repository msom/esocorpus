library(stringr)
library(rvest)
library(xml2)

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

  file <- "data-raw/Blavatsky_1877.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(paste0("https://www.theosociety.org/pasadena/isis/", link))
    page %>%
      html_elements("h5, h3, h4, blockquote, p:not(.link-list-0)") %>%
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

  file <- "data-raw/Blavatsky_1889_1.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(paste0("https://www.theosociety.org/pasadena/key/", link))
    page %>%
      html_elements("h5, h3, h4, blockquote, p:not(.link-list-0)") %>%
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

  file <- "data-raw/Blavatsky_1889_2.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(paste0("https://www.theosociety.org/pasadena/voice/", link))
    page %>%
      html_elements("h5, h3, h4, blockquote, p:not(.link-list-0)") %>%
      html_text() %>%
      str_squish() %>%
      cat(file = file, sep = "\n\n", append = TRUE)
  }
}

fetch_tarot_of_bohemians <- function() {
  #'
  #' Gets "The Tarot of the Bohemians"
  #'
  #' Only includes chapters, removes footnote signs and page numbers.
  #'
  page <- read_html("https://sacred-texts.com/tarot/tob/index.htm")
  links <- page %>%
    html_elements("a") %>%
    html_attr("href")
  links <- links[grepl("tob0[3-9]|tob[1-5]", links)]

  file <- "data-raw/Papus_1892.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(paste0("https://sacred-texts.com/tarot/tob/", link))
    page %>%
      as.character() %>%
      str_replace_all("<a.*</a>", "") %>%
      read_html() %>%
      html_elements("h1, h3, p:not(.link-list-0)") %>%
      html_text() %>%
      str_squish() %>%
      cat(file = file, sep = "\n\n", append = TRUE)
  }
}

fetch_spirits_book <- function() {
  #'
  #' Gets "The Spirits book"
  #'
  #' Only includes chapters.
  #'
  links <- read.table("links/kardec_spirits_book.txt")[, 1]
  file <- "data-raw/Kardec_1857.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(link)
    page %>%
      as.character() %>%
      str_replace_all("<strong>", "<p>") %>%
      str_replace_all("</strong>", "</p>") %>%
      read_html() %>%
      html_elements("div.texts div:not(.avancar-voltar)") %>%
      html_text2() %>%
      paste(sep = "\n\n", collapse = "\n\n") %>%
      str_split_1("\n\n") %>%
      str_unique() %>%
      cat(file = file, sep = "\n\n", append = TRUE)
  }
}

fetch_mediums_book <- function() {
  #'
  #' Gets "The Mediums' Book"
  #'
  #' Only includes chapters.
  #'
  links <- read.table("links/kardec_mediums_book")[, 1]
  file <- "data-raw/Kardec_1861.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(link)
    page %>%
      html_elements("div.texts div:not(.avancar-voltar)") %>%
      html_text2() %>%
      paste(sep = "\n\n", collapse = "\n\n") %>%
      str_replace_all("\r[^\n]", " ") %>%
      cat(file = file, append = TRUE)
  }
}


# Download texts
fetch_isis_unveiled()
fetch_key_to_theosophy()
fetch_voice_of_silence()
fetch_numbers()
fetch_tarot_of_bohemians()
fetch_spirits_book()
fetch_mediums_book()
