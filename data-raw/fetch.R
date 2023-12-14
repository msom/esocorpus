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
  prefix <- "https://kardecpedia.com/en/study-guide/2/the-spirits-book"
  links <- c(
    "/3/introduction",
    "/8/prolegomena",
    "/13/book-onefirst-causes/chapter-igod",
    "/64/book-onefirst-causes/chapter-iigeneral-elements-of-the-universe",
    "/65/book-onefirst-causes/chapter-iiicreation",
    "/66/book-onefirst-causes/chapter-ivthe-vital-principle",
    "/68/book-twothe-spirit-world/chapter-ispirits",
    "/69/book-twothe-spirit-world/chapter-iiincarnation-of-spirits",
    "/70/book-twothe-spirit-world/chapter-iiireturn-to-the-spirit-world-from-the-physical-life",
    "/71/book-twothe-spirit-world/chapter-ivmultiple-lives",
    "/72/book-twothe-spirit-world/chapter-vconsiderations-on-multiple-lives",
    "/73/book-twothe-spirit-world/chapter-vispirit-life",
    "/74/book-twothe-spirit-world/chapter-viireturn-to-physical-life",
    "/75/book-twothe-spirit-world/chapter-viiiemancipation-of-the-soul",
    "/76/book-twothe-spirit-world/chapter-ixintervention-of-spirits-on-the-physical-world",
    "/77/book-twothe-spirit-world/chapter-xspirit-occupations-and-missions",
    "/78/book-twothe-spirit-world/chapter-xithe-three-kingdoms",
    "/82/book-threemoral-laws/chapter-idivine-or-natural-law",
    "/83/book-threemoral-laws/chapter-iii-law-of-worship",
    "/84/book-threemoral-laws/chapter-iiiii-law-of-labor",
    "/85/book-threemoral-laws/chapter-iviii-law-of-reproduction",
    "/86/book-threemoral-laws/chapter-v-iv-the-law-of-preservation",
    "/87/book-threemoral-laws/chapter-viv-law-of-destruction",
    "/88/book-threemoral-laws/chapter-viivi-law-of-society",
    "/89/book-threemoral-laws/chapter-viiivii-law-of-progress",
    "/90/book-threemoral-laws/chapter-ixviii-law-of-equality",
    "/91/book-threemoral-laws/chapter-xix-law-of-liberty",
    "/92/book-threemoral-laws/chapter-xix-law-of-justice-love-and-charity",
    "/93/book-threemoral-laws/chapter-xiimoral-perfectlon",
    "/94/book-fourhope-and-consolation/chapter-iearthly-joys-and-sorrows",
    "/95/book-fourhope-and-consolation/chapter-iifuture-joys-and-sorrows",
    "/81/conclusion"
  )

  file <- "data-raw/Kardec_1857.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(paste0(prefix, link))
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
  prefix <- "https://mail.kardecpidia.com.br/en/study-guide/884/the-mediums-book/"
  links <- c(
    "945/introduction",
    "947/part-first-preliminary-observations/chapter-i-do-spirits-exist",
    "948/part-first-preliminary-observations/chapter-ii-the-marvellous-and-the-supernatural",
    "949/part-first-preliminary-observations/chapter-iii-plan-of-proceeding",
    "950/part-first-preliminary-observations/chapter-iv-theories",
    "952/part-second-spirit-manifestations/chapter-i-action-of-spirits-on-matter",
    "953/part-second-spirit-manifestations/chapter-ii-physical-manifestations-table-turning",
    "954/part-second-spirit-manifestations/chapter-iii-intelligent-manifestations",
    "955/part-second-spirit-manifestations/chapter-iv-theory-of-physical-manifestations",
    "956/part-second-spirit-manifestations/chapter-v-spontaneous-physical-manifestations",
    "957/part-second-spirit-manifestations/chapter-vi-visual-manifestations",
    "958/part-second-spirit-manifestations/chapter-vii-bi-corporeity-and-transfiguration",
    "959/part-second-spirit-manifestations/chapter-viii-laboratory-of-the-invisible-world",
    "960/part-second-spirit-manifestations/chapter-ix-haunted-places",
    "961/part-second-spirit-manifestations/chapter-x-nature-of-spirit-communications",
    "962/part-second-spirit-manifestations/chapter-xi-sematology-and-typtology",
    "963/part-second-spirit-manifestations/chapter-xii-pneumatography-or-direct-writing-pneumatophony-or-direct-spirit-sounds",
    "964/part-second-spirit-manifestations/chapter-xiii-psychography",
    "965/part-second-spirit-manifestations/chapter-xiv-of-mediums",
    "966/part-second-spirit-manifestations/chapter-xv-writing-or-psychographic-mediums",
    "967/part-second-spirit-manifestations/chapter-xvi-special-mediums",
    "968/part-second-spirit-manifestations/chapter-xvii-formation-of-mediums",
    "969/part-second-spirit-manifestations/chapter-xviii-inconveniences-and-dangers-of-mediumship",
    "970/part-second-spirit-manifestations/chapter-xix-role-of-the-medium-in-spirit-communications",
    "971/part-second-spirit-manifestations/chapter-x-moral-influence-of-the-medium",
    "972/part-second-spirit-manifestations/chapter-xxi-influence-of-the-surroundings",
    "973/part-second-spirit-manifestations/chapter-xxii-of-mediumship-in-animals",
    "974/part-second-spirit-manifestations/chapter-xxiii-on-obsession",
    "975/part-second-spirit-manifestations/chapter-xxiv-identity-of-spirits",
    "976/part-second-spirit-manifestations/chapter-xxv-on-invocations",
    "977/part-second-spirit-manifestations/chapter-xxvi-questions-that-may-be-addressed-to-spirits",
    "978/part-second-spirit-manifestations/chapter-xxvii-contradictions-and-mystifications",
    "979/part-second-spirit-manifestations/chapter-xxviii-charlatanism-and-jugglery",
    "980/part-second-spirit-manifestations/chapter-xxix-reunions-and-spiritist-societies",
    "981/part-second-spirit-manifestations/regulations-of-the-parisian-society-for-spiritist-studies",
    "982/part-second-spirit-manifestations/chapter-xxxi-dissertations-by-spirits",
    "983/part-second-spirit-manifestations/chapter-xxxii-spiritist-vocabulary"
  )

  file <- "data-raw/Kardec_1861.txt"
  cat("", file = file)
  for (link in links) {
    page <- read_html(paste0(prefix, link))
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
