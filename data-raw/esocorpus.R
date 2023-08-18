library(readtext)
library(stringr)
library(usethis)

remove_gutenberg_metadata <- function(text) {
  # Returns everything between the start and end markers
  result <- str_split_1(text, "\\*\\*\\*START OF .*\\*\\*\\*")
  if (length(result) > 1) {
    result <- result[[2]]
  }
  result <- str_split_1(result, "\\*\\*\\*END OF .*\\*\\*\\*")[[1]]
  result <- str_trim(result)
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

# Save
use_data(esocorpus, overwrite = TRUE)



