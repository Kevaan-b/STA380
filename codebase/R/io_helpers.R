read_model_txt <- function(path) {
  out <- read.table(
    file = path,
    header = TRUE,
    sep = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  out
}

write_model_txt <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(
    x = data,
    file = path,
    sep = " ",
    quote = FALSE,
    row.names = FALSE
  )
  invisible(path)
}
