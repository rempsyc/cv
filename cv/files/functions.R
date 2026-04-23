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

latex_text_size <- function(size, leading = 1.15) {
  if (is.null(size) || length(size) == 0 || is.na(size)) {
    return("")
  }

  if (is.numeric(size)) {
    line_height <- round(size * leading, 2)
    return(sprintf("\\fontsize{%spt}{%spt}\\selectfont", size, line_height))
  }

  size_chr <- trimws(as.character(size))
  if (grepl("^[0-9]+(\\.[0-9]+)?$", size_chr)) {
    size_num <- as.numeric(size_chr)
    line_height <- round(size_num * leading, 2)
    return(sprintf("\\fontsize{%spt}{%spt}\\selectfont", size_num, line_height))
  }

  size_chr
}

latex_scaled_text_size <- function(size, factor = 1, leading = 1.15) {
  if (is.null(size) || length(size) == 0 || is.na(size)) {
    return("")
  }

  if (is.numeric(size)) {
    return(latex_text_size(size * factor, leading = leading))
  }

  size_chr <- trimws(as.character(size))
  if (grepl("^[0-9]+(\\.[0-9]+)?$", size_chr)) {
    return(latex_text_size(as.numeric(size_chr) * factor, leading = leading))
  }

  size_chr
}

protect_date_range <- function(x) {
  ifelse(
    is.na(x) | x == "",
    x,
    ifelse(
      grepl("^\\\\textbf\\{\\d{4}-\\d{4}\\}$", x),
      sub(
        "^\\\\textbf\\{(\\d{4})-(\\d{4})\\}$",
        "\\\\textbf{\\\\mbox{\\1--\\2}}",
        x
      ),
      ifelse(
        grepl("^\\d{4}-\\d{4}$", x),
        paste0("\\mbox{", gsub("-", "--", x), "}"),
        x
      )
    )
  )
}

get_table_left_indent <- function(default = "0.3cm") {
  indent <- get0("table_left_indent", ifnotfound = default, inherits = TRUE)
  as.character(indent)
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

nice_awards <- function(data, theme_color = headcolor, language = "EN", number = TRUE) {
  total <- "Total Amount Offered"
  if (language != "EN") {
    data <- data %>%
      mutate(Name = ifelse(is.na(Name_alt), Name, Name_alt))
    total <- "Montant total offert"
  }

  n <- nrow(data)
  data$Name <- sapply(seq_len(n), function(i) {
    if (isTRUE(number)) {
      num <- (n + 1 - i)
      return(paste0(num, ". ", data$Name[i]))
    }
    data$Name[i]
  })

  total_award <- data$Amount

  first_year <- min(as.numeric(unlist(strsplit(data$Date, "-"))))
  last_year <- max(as.numeric(unlist(strsplit(data$Date, "-"))))

  data <- data %>%
    add_row(
      Name = bold(total),
      URL = "",
      Date = paste0(first_year, "-", last_year),
      Amount = sum(total_award, na.rm = TRUE),
      Declined = FALSE,
      .before = 1
    ) %>%
    mutate(
      Date = protect_date_range(Date),
      Name = link(
        paste0("\\hspace{", get_table_left_indent(), "} ", Name, "\\dotfill"),
        URL,
        color = "333333"
      ),
      Amount = paste0("\\$", scales::label_comma(accuracy = 1)(Amount)),
      Amount = ifelse(Declined == TRUE, paste0("(", Amount, ")"), Amount),
      Amount = ifelse(Amount == "\\$NA", "—", Amount)
    )

  # Bold total amount (must do later since we have symbol processing)
  data[1, "Date"] <- bold(data[1, "Date"])
  data[1, "Amount"] <- bold(data[1, "Amount"])

  data[1, 1] <- gsub("333333", theme_color, data[1, 1]) #Red: FF0000, but we use same colour as rest

  brief_entries(data, Name, Date, Amount, .protect = FALSE)
}

nice_grants <- function(data, theme_color = headcolor, language = "EN", number = TRUE) {
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
    } else if (!isTRUE(number)) {
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
      Date = paste0(first_year, "-", last_year),
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
      Date = protect_date_range(Date),
      dotfill = ifelse(secondary, "", "\\dotfill"),
      Name = link(
        paste0("\\hspace{", get_table_left_indent(), "} ", Name, dotfill),
        URL,
        color = "333333"
      ),
      Amount = paste0("\\$", scales::label_comma(accuracy = 1)(Amount)),
      Amount = ifelse(Amount == "\\$NA", "", Amount)
    )

  # Bold total amount (must do later since we have symbol processing)
  data[1, "Date"] <- bold(data[1, "Date"])
  data[1, "Amount"] <- bold(data[1, "Amount"])

  data[1, 1] <- gsub("333333", theme_color, data[1, 1]) #Red: FF0000, but we use same colour as rest

  brief_entries(data, what = Name, when = Date, with = Amount, .protect = FALSE)
}

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

gsub_language <- function(text, dictionary) {
  for (i in seq_len(nrow(dictionary))) {
    text <- gsub(
      dictionary$English[i],
      dictionary$French[i],
      text,
      fixed = TRUE
    )
  }
  gsub(
    " \\[[^]]*\\]",
    "",
    text
  )
}

month_map <- tribble(
  ~English,    ~French,
  "January",   "janvier",
  "February",  "février",
  "March",     "mars",
  "April",     "avril",
  "May",       "mai",
  "June",      "juin",
  "July",      "juillet",
  "August",    "août",
  "September", "septembre",
  "October",   "octobre",
  "November",  "novembre",
  "December",  "décembre"
)

number_pubs <- function(
  pubs,
  author_bold = NULL,
  language = "EN",
  language_dictionary = NULL,
  number = TRUE
) {
  if (language != "EN") {
    language_dictionary <- rbind(language_dictionary, month_map)
    pubs <- gsub_language(pubs, language_dictionary)
  }
  if (!is.null(author_bold)) {
    pubs <- gsub(author_bold, paste0("**", author_bold, "**"), pubs)
  }
  if (isTRUE(number)) {
    pubs <- paste0(rev(seq_along(pubs)), "\\. ", pubs)
  }
  cat("\\cvwidehang\n")
  cat(pubs, sep = "\n\n\n")
}

number_prefix <- function(index, number = TRUE) {
  if (isTRUE(number)) {
    return(paste0(index, "\\. "))
  }
  ""
}

nice_header <- function(header, header_alt, language = "EN") {
  if (language != "EN") {
    header <- header_alt
  }
  cat(paste("#", header))
}

if_english <- function(x, y, language = language) {
  ifelse(language == "EN", x, y)
}

nice_entries <- function(data, language = "EN", language_dictionary = NULL) {
  if (language != "EN") {
    data$what <- gsub_language(data$what, language_dictionary)
    data$with <- gsub_language(data$with, language_dictionary)
    data$details <- gsub_language(data$details, language_dictionary)
  }
  detailed_entries(data, with, when, what, where, details, .protect = FALSE)
}
