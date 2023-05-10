# To format links throughout the CV
link <- function(txt, url, color = "1976D2") {
  paste0("\\href{", url, "}{\\color[HTML]{", color, "}{", txt, "}}")
}