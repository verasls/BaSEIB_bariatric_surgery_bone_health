# Write output
#
# Writes a data.frame to a csv file in the output directory with the specified
#   name. If the output directory does not exist, it will be created in the
#   project root directory.
#
# Args:
#   data: A data.frame.
#   name: A character string.
write_output <- function(data, name) {
  if (!dir.exists(here::here("output/"))) {
    dir.create(here::here("output"))
  }
  name <- paste0(here::here(), "/output/", name)
  readr::write_csv(data, name)
}
