# install.packages("stringr")
# install.packages("rvest")

library(stringr)
library(rvest)
library(xml2)

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
      as.character() %>%
      str_replace_all("<strong>", "<p>") %>%
      str_replace_all("</strong>", "</p>") %>%
      read_html() %>%
      html_elements('div.texts div:not(.avancar-voltar)') %>%
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
  links <- c(
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/945/introduction",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/947/part-first-preliminary-observations/chapter-i-do-spirits-exist",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/948/part-first-preliminary-observations/chapter-ii-the-marvellous-and-the-supernatural",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/949/part-first-preliminary-observations/chapter-iii-plan-of-proceeding",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/950/part-first-preliminary-observations/chapter-iv-theories",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/952/part-second-spirit-manifestations/chapter-i-action-of-spirits-on-matter",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/953/part-second-spirit-manifestations/chapter-ii-physical-manifestations-table-turning",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/954/part-second-spirit-manifestations/chapter-iii-intelligent-manifestations",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/955/part-second-spirit-manifestations/chapter-iv-theory-of-physical-manifestations",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/956/part-second-spirit-manifestations/chapter-v-spontaneous-physical-manifestations",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/957/part-second-spirit-manifestations/chapter-vi-visual-manifestations",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/958/part-second-spirit-manifestations/chapter-vii-bi-corporeity-and-transfiguration",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/959/part-second-spirit-manifestations/chapter-viii-laboratory-of-the-invisible-world",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/960/part-second-spirit-manifestations/chapter-ix-haunted-places",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/961/part-second-spirit-manifestations/chapter-x-nature-of-spirit-communications",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/962/part-second-spirit-manifestations/chapter-xi-sematology-and-typtology",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/963/part-second-spirit-manifestations/chapter-xii-pneumatography-or-direct-writing-pneumatophony-or-direct-spirit-sounds",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/964/part-second-spirit-manifestations/chapter-xiii-psychography",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/965/part-second-spirit-manifestations/chapter-xiv-of-mediums",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/966/part-second-spirit-manifestations/chapter-xv-writing-or-psychographic-mediums",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/967/part-second-spirit-manifestations/chapter-xvi-special-mediums",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/968/part-second-spirit-manifestations/chapter-xvii-formation-of-mediums",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/969/part-second-spirit-manifestations/chapter-xviii-inconveniences-and-dangers-of-mediumship",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/970/part-second-spirit-manifestations/chapter-xix-role-of-the-medium-in-spirit-communications",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/971/part-second-spirit-manifestations/chapter-x-moral-influence-of-the-medium",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/972/part-second-spirit-manifestations/chapter-xxi-influence-of-the-surroundings",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/973/part-second-spirit-manifestations/chapter-xxii-of-mediumship-in-animals",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/974/part-second-spirit-manifestations/chapter-xxiii-on-obsession",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/975/part-second-spirit-manifestations/chapter-xxiv-identity-of-spirits",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/976/part-second-spirit-manifestations/chapter-xxv-on-invocations",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/977/part-second-spirit-manifestations/chapter-xxvi-questions-that-may-be-addressed-to-spirits",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/978/part-second-spirit-manifestations/chapter-xxvii-contradictions-and-mystifications",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/979/part-second-spirit-manifestations/chapter-xxviii-charlatanism-and-jugglery",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/980/part-second-spirit-manifestations/chapter-xxix-reunions-and-spiritist-societies",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/981/part-second-spirit-manifestations/regulations-of-the-parisian-society-for-spiritist-studies",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/982/part-second-spirit-manifestations/chapter-xxxi-dissertations-by-spirits",
    "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/983/part-second-spirit-manifestations/chapter-xxxii-spiritist-vocabulary"
  )

  file <- "data-raw/Kardec_1861.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(link)
    page %>%
      html_elements('div.texts div:not(.avancar-voltar)') %>%
      html_text2() %>%
      paste(sep = "\n\n", collapse = "\n\n") %>%
      str_replace_all("\r[^\n]", " ") %>%
      cat(file = file, append = TRUE)
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
fetch_spirits_book()
fetch_mediums_book()
