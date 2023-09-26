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

  file <- "data-raw/Blavatsky_1877.txt"
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

  file <- "data-raw/Blavatsky_1889_1.txt"
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

  file <- "data-raw/Blavatsky_1889_2.txt"
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

fetch_tarot_of_bohemians <- function() {
  #'
  #' Gets "The Tarot of the Bohemians"
  #'
  #' Only includes chapters.
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
      html_elements('h1, h3, p:not(.link-list-0)') %>%
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
  links <- c(
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/3/introduction",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/8/prolegomena",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/13/book-onefirst-causes/chapter-igod",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/64/book-onefirst-causes/chapter-iigeneral-elements-of-the-universe",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/65/book-onefirst-causes/chapter-iiicreation",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/66/book-onefirst-causes/chapter-ivthe-vital-principle",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/68/book-twothe-spirit-world/chapter-ispirits",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/69/book-twothe-spirit-world/chapter-iiincarnation-of-spirits",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/70/book-twothe-spirit-world/chapter-iiireturn-to-the-spirit-world-from-the-physical-life",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/71/book-twothe-spirit-world/chapter-ivmultiple-lives",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/72/book-twothe-spirit-world/chapter-vconsiderations-on-multiple-lives",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/73/book-twothe-spirit-world/chapter-vispirit-life",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/74/book-twothe-spirit-world/chapter-viireturn-to-physical-life",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/75/book-twothe-spirit-world/chapter-viiiemancipation-of-the-soul",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/76/book-twothe-spirit-world/chapter-ixintervention-of-spirits-on-the-physical-world",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/77/book-twothe-spirit-world/chapter-xspirit-occupations-and-missions",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/78/book-twothe-spirit-world/chapter-xithe-three-kingdoms",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/82/book-threemoral-laws/chapter-idivine-or-natural-law",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/83/book-threemoral-laws/chapter-iii-law-of-worship",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/84/book-threemoral-laws/chapter-iiiii-law-of-labor",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/85/book-threemoral-laws/chapter-iviii-law-of-reproduction",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/86/book-threemoral-laws/chapter-v-iv-the-law-of-preservation",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/87/book-threemoral-laws/chapter-viv-law-of-destruction",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/88/book-threemoral-laws/chapter-viivi-law-of-society",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/89/book-threemoral-laws/chapter-viiivii-law-of-progress",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/90/book-threemoral-laws/chapter-ixviii-law-of-equality",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/91/book-threemoral-laws/chapter-xix-law-of-liberty",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/92/book-threemoral-laws/chapter-xix-law-of-justice-love-and-charity",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/93/book-threemoral-laws/chapter-xiimoral-perfectlon",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/94/book-fourhope-and-consolation/chapter-iearthly-joys-and-sorrows",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/95/book-fourhope-and-consolation/chapter-iifuture-joys-and-sorrows",
    "https://kardecpedia.com/en/study-guide/2/the-spirits-book/81/conclusion"
  )

  file <- "data-raw/Kardec_1857.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(link)
    page %>%
      html_elements('div.texts div:not(.avancar-voltar)') %>%
      html_text2() %>%
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
fetch_tarot_of_bohemians()
