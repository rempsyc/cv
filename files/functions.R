# Define language parameters
English <- language == "EN"
French <- language == "FR"

# To format links throughout the CV
link <- function(txt, url, color = "1976D2") {
  if (!is.null(color)) {
    paste0("\\href{", url, "}{\\color[HTML]{", color, "}{", txt, "}}")
  } else {
    paste0("\\href{", url, "}{", txt, "}")
  }
}

bold <- function(txt) {
  paste0("\\textbf{", txt, "}")
}

italics <- function(txt) {
  paste0("\\textit{", txt, "}")
}

# ------------------------------
# Scholar data
# ------------------------------
get_scholar_name <- function(scholar.profile) {
  scholar::get_publications(scholar.profile, flush = TRUE) %>%
    mutate(author = scholar::get_complete_authors(scholar.profile, pubid)) %>%
    select(author)
}

create_grant_entry <- function(
  Funder_Name,
  Project_Name,
  Funder_Name_alt = NULL,
  Project_Name_alt = NULL,
  URL = "",
  Date,
  Amount,
  Program = ""
) {
  a <- tibble(
    Name = paste0("\\textbf{", Funder_Name, "}"),
    Name_alt = ifelse(
      is.null(Funder_Name_alt),
      NA,
      paste0("\\textbf{", Funder_Name_alt, "}")
    ),
    URL = "",
    Details = "",
    Date = Date,
    Amount = Amount,
    Activity = NA,
    Program = Program,
    secondary = FALSE
  )

  b <- tibble(
    Name = paste0("\\textit{", Project_Name, "}"),
    Name_alt = ifelse(
      is.null(Project_Name_alt),
      NA,
      paste0("\\textit{", Project_Name_alt, "}")
    ),
    URL = URL,
    Details = "",
    Date = "",
    Amount = NA,
    Activity = NA,
    Program = Program,
    secondary = TRUE
  )

  rbind(a, b)
}

nice_awards <- function(data, theme_color = headcolor, language = "EN") {
  total <- "Total Amount Offered"
  if (language != "EN") {
    data <- data %>%
      mutate(Name = ifelse(is.na(Name_alt), Name, Name_alt))
    total <- "Montant total offert"
  }

  n <- nrow(data)
  data$Name <- sapply(seq_len(n), function(i) {
    num <- (n + 1 - i)
    paste0(num, ". ", data$Name[i])
  })

  total_award <- data$Amount

  first_year <- min(as.numeric(unlist(strsplit(data$Date, "-"))))
  last_year <- max(as.numeric(unlist(strsplit(data$Date, "-"))))

  data <- data %>%
    add_row(
      Name = bold(total),
      URL = "",
      Date = bold(paste0(first_year, "-", last_year)),
      Amount = sum(total_award, na.rm = TRUE),
      Declined = FALSE,
      .before = 1
    ) %>%
    mutate(
      Name = link(
        paste0("\\hspace{0.5cm} ", Name, "\\dotfill"),
        URL,
        color = "333333"
      ),
      Amount = paste0("\\$", scales::label_comma(accuracy = 1)(Amount)),
      Amount = ifelse(Declined == TRUE, paste0("(", Amount, ")"), Amount),
      Amount = ifelse(Amount == "\\$NA", "—", Amount)
    )

  # Bold total amount (must do later since we have symbol processing)
  data[1, "Amount"] <- bold(data[1, "Amount"])

  data[1, 1] <- gsub("333333", theme_color, data[1, 1]) #Red: FF0000, but we use same colour as rest

  brief_entries(data, Name, Date, Amount, .protect = FALSE)
}

nice_grants <- function(data, theme_color = headcolor, language = "EN") {
  total <- "Total Amount Offered"
  if (language != "EN") {
    data <- data %>%
      mutate(Name = ifelse(is.na(Name_alt), Name, Name_alt))
    total <- "Montant total offert"
  }

  n <- nrow(data)
  data$Name <- sapply(seq_len(n), function(i) {
    if (i %% 2 == 0) {
      data$Name[i]
    } else {
      num <- (n + 1 - i) / 2 # 1 → 7, 3 → 6, ..., 13 → 1
      gsub(
        "\\textbf{",
        paste0("\\textbf{", num, ". "),
        data$Name[i],
        fixed = TRUE
      )
    }
  })

  total_grant <- data$Amount

  first_year <- min(as.numeric(unlist(strsplit(data$Date, "-"))))
  last_year <- max(as.numeric(unlist(strsplit(data$Date, "-"))))

  data <- data %>%
    add_row(
      Name = bold(total),
      URL = "",
      Date = bold(paste0(first_year, "-", last_year)),
      Amount = sum(total_grant, na.rm = TRUE),
      secondary = FALSE,
      .before = 1
    ) %>%
    # add_row(Name = "Total Grant + Award Offered",
    #         URL = "",
    #         Date = "",
    #         Amount = sum(total_grant, total_award, na.rm = TRUE),
    #         secondary = FALSE) %>%
    mutate(
      dotfill = ifelse(secondary, "", "\\dotfill"),
      Name = link(
        paste0("\\hspace{0.5cm} ", Name, dotfill),
        URL,
        color = "333333"
      ),
      Amount = paste0("\\$", scales::label_comma(accuracy = 1)(Amount)),
      Amount = ifelse(Amount == "\\$NA", "", Amount)
    )

  # Bold total amount (must do later since we have symbol processing)
  data[1, "Amount"] <- bold(data[1, "Amount"])

  data[1, 1] <- gsub("333333", theme_color, data[1, 1]) #Red: FF0000, but we use same colour as rest

  brief_entries(data, what = Name, when = Date, with = Amount, .protect = FALSE)
}

scale_awesomecv_fonts <- function(
  scale = 1,
  file_path = "awesome-cv.cls",
  backup = TRUE
) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  if (backup) {
    file.copy(file_path, paste0(file_path, ".bak"), overwrite = TRUE)
  }
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

format.authors <- function(scholar.profile, author.name) {
  swap_initials <- function(author.name) {
    sub("(.*) (.*)", "\\2, \\1.", trimws(author.name))
  }

  pubs <- scholar::get_publications(scholar.profile)
  pubs %>%
    strsplit(x = .$author, split = ",") -> pubs2
  lapply(pubs2, function(x) {
    x <- swap_initials(x)
    x[length(x)] <- paste0("& ", x[length(x)])
    x <- paste0(x, collapse = ", ")
    ifelse(startsWith(x, "& "), sub("& ", "", x), x)
  }) -> pubs$author

  author.name2 <- swap_initials(author.name)

  pubs %>%
    arrange(desc(year)) %>%
    mutate(
      journal = paste0("*", journal, "*"),
      Publications = paste0(
        author,
        " (",
        year,
        "). ",
        title,
        ". ",
        journal,
        ". ",
        number
      ),
      Publications = gsub(
        author.name2,
        paste0("**", author.name2, "**"),
        Publications
      )
    ) %>%
    select(Publications)
}

number_pubs <- function(pubs, author_bold = NULL) {
  if (!is.null(author_bold)) {
    pubs <- gsub(author_bold, paste0("**", author_bold, "**"), pubs)
  }
  pubs <- paste0(rev(seq_along(pubs)), "\\. ", pubs)
  cat(pubs, sep = "\n\n\n")
}

nice_header <- function(header, header_alt, language = "EN") {
  if (language != "EN") {
    header <- header_alt
  }
  cat(paste("#", header))
}
