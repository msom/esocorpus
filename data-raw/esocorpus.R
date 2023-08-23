library(readtext)
library(stringr)
library(usethis)

remove_gutenberg_metadata <- function(text) {
  # Returns everything between the start and end markers
  result <- str_split_1(text, "\\*\\*\\*.*\\*\\*\\*")
  if (length(result) > 1) {
    result <- result[[2]]
  }
  result <- str_split_1(result, "\\*\\*\\*.*\\*\\*\\*")[[1]]
  result <- str_trim(result)
  return(result)
}

remove_hathitrust_metadata <- function(text) {
  # Returns everything after the generated line
  result <- str_split_1(text, "Generated on .+ GMT")
  if (length(result) > 1) {
    result <- result[[2]]
  }
  return(result)
}

# Read texts
esocorpus = readtext(
  "data-raw/*.[!R]*",
  docvarsfrom = "filenames",
  dvsep = "_",
  docvarnames = c("name", "first.name", "year", "title")
)

# Cleanup
esocorpus$text <- lapply(esocorpus$text, remove_gutenberg_metadata)
esocorpus$text <- lapply(esocorpus$text, remove_hathitrust_metadata)

# Save
use_data(esocorpus, overwrite = TRUE)
