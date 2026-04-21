# -----------------------------------------------
# Package loading with automatic installation
# -----------------------------------------------
# These packages are required for the CV template.
# Missing packages will be installed automatically.

required_packages <- c(
  "vitae", # CV template framework
  "tidyverse", # Data science toolkit (includes dplyr, ggplot2, etc.)
  "scholar", # Google Scholar data
  "patchwork", # Combine ggplots
  "see", # Model visualisation (theme_modern, scale_fill_material_d)
  "rsvg", # SVG to PNG conversion (for CRAN badges)
  "kableExtra", # Enhanced tables
  "here" # Robust file paths
)

missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

invisible(lapply(required_packages, library, character.only = TRUE))
