extract_functions <- function(paths) {
  code_lines <- character(0)

  for (path in paths) {
    lines <- readLines(path, warn = FALSE)
    in_chunk <- FALSE

    for (line in lines) {
      # Line is start of code chunk
      if (grepl("^```\\{r", line)) {
        in_chunk <- TRUE
        next
      }

      # Line is end of code chunk
      if (in_chunk && grepl("^```\\s*$", line)) {
        in_chunk <- FALSE
        next
      }

      # In a code chunk
      if (in_chunk) {
        # Skip lines where the first character is #
        if (grepl("^\\s*#", line)) {
          next
        }

        # Skip blank lines
        if (grepl("^\\s*$", line)) {
          next
        }

        # If all checks pass, add to code_lines
        code_lines <- c(code_lines, line)
      }
    }
  }

  # Match function calls: function( or package::function(
  pattern <- "(\\S+::)?\\S+\\("
  matches <- gregexpr(pattern, code_lines, perl = TRUE)
  func_calls <- unlist(regmatches(code_lines, matches))

  # Remove ( and whitespace
  func_calls <- sub("\\s*\\($", "", func_calls)

  tbl <- as.data.frame(table(Function = func_calls))
  names(tbl)[2] <- "Count"
  tbl <- tbl[order(-tbl$Count), ]
  rownames(tbl) <- NULL
  tbl
}
