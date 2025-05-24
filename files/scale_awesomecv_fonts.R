scale_awesomecv_fonts <- function(scale = 1, file_path = "awesome-cv.cls", backup = TRUE) {
  if (!file.exists(file_path)) stop("File not found: ", file_path)
  if (backup) file.copy(file_path, paste0(file_path, ".bak"), overwrite = TRUE)
  lines <- readLines(file_path)
  # Only scale first value, keep {1em}
  pattern <- "\\\\fontsize\\{([0-9.]+)pt\\}\\{1em\\}"
  lines_scaled <- gsubfn::gsubfn(
    pattern,
    function(size = pattern) {
      size <- as.numeric(size)
      scaled <- round(size * scale, 2)
      paste0("\\fontsize{", scaled, "pt}{1em}")
    },
    lines
  )
  writeLines(lines_scaled, file_path)
  message("Font sizes scaled, baselineskip unchanged (1em).")
}
# 
# 
# pattern <- "\\\\fontsize\\{([0-9.]+)pt\\}\\{1em\\}"
# 
# paste0("fontsize{", scaled, "pt}{1em}")
# 
# lines_scaled <- gsubfn::gsubfn(
#   pattern,
#   function(size = pattern) {
#     size <- as.numeric(size)
#     scaled <- round(size * scale, 2)
#     paste0("fontsize{", scaled, "pt}{1em}")
#   },
#   lines
# )
# lines_scaled[194]


