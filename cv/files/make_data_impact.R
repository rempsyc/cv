# ------------------------------
# Scholar data (with local caching to avoid rate-limiting)
# ------------------------------

# Build stable exclusion keys of the form "LastName (Year)" with letter suffixes
# for duplicates in the same year, e.g., "Smith (2024b)".
build_scholar_publication_keys <- function(publications_df) {
  if (is.null(publications_df) || nrow(publications_df) == 0) {
    return(publications_df)
  }

  data <- publications_df %>%
    mutate(
      Authors = paste0(lapply(stringr::str_split(author, " "), `[[`, 2)),
      Authors = stringr::str_to_title(stringr::str_remove_all(Authors, ",")),
      PublicationKey = paste0(Authors, " (", year)
    )

  suffix <- letters
  suffix[1] <- ""
  for (i in seq_len(nrow(data))) {
    key <- data$PublicationKey[i]
    j <- 1
    while (paste0(key, suffix[j]) %in% data$PublicationKey[seq_len(i - 1)]) {
      j <- j + 1
    }
    data$PublicationKey[i] <- paste0(key, suffix[j])
  }

  data %>%
    mutate(Publication = paste0(PublicationKey, ")"))
}

# Normalize exclusion keys to make matching robust to dash variants and spacing.
normalize_publication_key <- function(x) {
  x <- as.character(x)
  # Convert common Unicode dash characters to ASCII hyphen.
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

# Print a compact preview table to help tune `scholar_exclude`.
# Returns the exclusion keys invisibly for quick copy/paste.
preview_scholar_exclusions <- function(
  data_scholar,
  scholar_exclude = NULL,
  n = Inf,
  order_by = c("year", "cites"),
  show_excluded = FALSE
) {
  order_by <- match.arg(order_by)

  pubs <- data_scholar$scholar_publications_all
  if (is.null(pubs) || nrow(pubs) == 0) {
    message("No Google Scholar publications available for exclusion preview.")
    return(invisible(character(0)))
  }

  preview <- pubs %>%
    mutate(
      Excluded = ifelse(
        normalize_publication_key(Publication) %in%
          normalize_publication_key(scholar_exclude),
        "yes",
        "no"
      )
    ) %>%
    select(Publication, year, cites, Excluded, title, journal)

  if (!show_excluded) {
    preview <- preview %>% filter(Excluded == "no")
  }

  if (order_by == "year") {
    preview <- preview %>% arrange(desc(year), desc(cites))
  } else {
    preview <- preview %>% arrange(desc(cites), desc(year))
  }

  if (is.finite(n)) {
    preview <- head(preview, n)
  }

  preview <- preview %>%
    mutate(N = dplyr::n() - dplyr::row_number() + 1) %>%
    select(N, Publication, year, cites, Excluded, title, journal)

  cat(
    knitr::kable(
      preview,
      format = "latex",
      booktabs = TRUE,
      escape = TRUE,
      align = c("c", "l", "c", "c", "c", "l", "l"),
      col.names = c("#", "Exclude key", "Year", "Cites", "In scholar_exclude", "Title", "Journal")
    ),
    sep = "\n"
  )

  invisible(preview$Publication)
}

# Cache settings
# Set `refresh_scholar = TRUE` before sourcing this file to force a refresh.
# Otherwise, cached data is used if available and less than `cache_days` old.
if (!exists("refresh_scholar")) {
  refresh_scholar <- FALSE
}
if (!exists("cache_days")) {
  cache_days <- 1
}
cache_file <- here::here("files", "scholar_cache.rds")

fetch_scholar_data <- function(scholar.profile, author.name) {
  data <- list()
  data[["date"]] <- format(Sys.time(), "%d %B %Y")

  message("Fetching Google Scholar data...")
  data[["scholar_stats"]] <- tryCatch(
    scholar::get_profile(scholar.profile),
    error = function(e) {
      warning("Could not fetch Google Scholar profile: ", e$message)
      list(h_index = NA, total_cites = NA)
    }
  )

  data[["scholar_history"]] <- tryCatch(
    scholar::get_citation_history(scholar.profile),
    error = function(e) {
      warning("Could not fetch citation history: ", e$message)
      data.frame(year = integer(), cites = integer())
    }
  )

  data[["scholar_publications"]] <- tryCatch(
    scholar::get_publications(scholar.profile, flush = TRUE),
    error = function(e) {
      warning("Could not fetch publications: ", e$message)
      data.frame(
        title = character(),
        author = character(),
        journal = character(),
        number = character(),
        cites = integer(),
        year = integer(),
        pubid = character()
      )
    }
  )

  # Only fetch complete authors for pubs where the list is truncated ("...")
  # or where the target author name is missing. This avoids one HTTP request

  # per publication and typically cuts requests from ~30 to ~5-10.
  if (nrow(data[["scholar_publications"]]) > 0) {
    needs_full <- grepl("\\.\\.\\.", data[["scholar_publications"]]$author) |
      !grepl(author.name, tolower(data[["scholar_publications"]]$author))
    message(
      "Fetching complete authors for ",
      sum(needs_full),
      "/",
      nrow(data[["scholar_publications"]]),
      " publications..."
    )
    if (any(needs_full)) {
      pubids_to_fetch <- data[["scholar_publications"]]$pubid[needs_full]
      capture.output(
        full_authors <- tryCatch(
          scholar::get_complete_authors(scholar.profile, pubids_to_fetch),
          error = function(e) {
            warning("Could not fetch complete authors: ", e$message)
            rep(NA_character_, length(pubids_to_fetch))
          }
        )
      )
      data[["scholar_publications"]]$author[needs_full] <- full_authors
    }
  }

  data
}

cache_is_valid <- file.exists(cache_file) &&
  difftime(Sys.time(), file.mtime(cache_file), units = "days") < cache_days &&
  !refresh_scholar

if (cache_is_valid) {
  message(
    "Loading cached Google Scholar data (",
    round(
      difftime(Sys.time(), file.mtime(cache_file), units = "hours"),
      1
    ),
    "h old). Set refresh_scholar <- TRUE to update."
  )
  data_scholar <- readRDS(cache_file)
} else {
  data_scholar <- fetch_scholar_data(scholar.profile, author.name)
  saveRDS(data_scholar, cache_file)
  message("Google Scholar data cached to: ", cache_file)
}

# Correct author name inconsistencies using config from cv.Rmd
if (exists("scholar_name_fixes") && length(scholar_name_fixes) > 0) {
  for (i in seq_along(scholar_name_fixes)) {
    data_scholar$scholar_publications$author <- gsub(
      names(scholar_name_fixes)[i],
      scholar_name_fixes[i],
      data_scholar$scholar_publications$author,
      fixed = TRUE
    )
  }
}

if (
  nrow(data_scholar[["scholar_publications"]]) > 0 &&
    nrow(data_scholar[["scholar_history"]]) > 0
) {
  data_scholar[["scholar_data"]] <- data_scholar[["scholar_publications"]] %>%
    dplyr::filter(year > 1950) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      Publications = n(),
    ) %>%
    dplyr::mutate(
      Publications = cumsum(Publications)
    ) %>%
    dplyr::rename(Year = year) %>%
    tidyr::gather(Index, Number, -Year) %>%
    dplyr::mutate(Index = forcats::fct_rev(Index)) %>%
    rbind(
      data_scholar[["scholar_history"]] %>%
        dplyr::rename(Number = cites, Year = year) %>%
        mutate(Index = "Citations", Number = cumsum(Number))
    )
} else {
  data_scholar[["scholar_data"]] <- data.frame(
    Year = integer(),
    Number = numeric(),
    Index = character()
  )
}

# Publications individual
data <- build_scholar_publication_keys(data_scholar[["scholar_publications"]]) %>%
  mutate(Journal = stringr::str_to_title(journal))

# Keep a complete list (pre-filter) so exclusions can be previewed in the CV.
data_scholar[["scholar_publications_all"]] <- data

# Ignore publications specified in cv.Rmd config
if (exists("scholar_exclude")) {
  ignored.publications <- scholar_exclude
} else {
  ignored.publications <- c()
}

ignored.publications.normalized <- normalize_publication_key(ignored.publications)

# Filter some publications out, as desired.
# IMPORTANT: this filtered table is used for publication-count metrics only
# (n-publications and author-position counts). Profile-level metrics such as
# h-index and total citations are always taken from `scholar_stats`.
data_scholar[["scholar_publications"]] <- data %>%
  mutate(
    Publication = fct_reorder(Publication, cites, .desc = TRUE)
  ) %>%
  filter(!normalize_publication_key(as.character(Publication)) %in% ignored.publications.normalized)

# Manual correction for non-publications
# (You might need to change this yourself!)
row.to.correct <- nrow(data_scholar[["scholar_publications"]]) / 2
# We divide the total number of rows by 2 here because we want the last year
# of publications, which sometimes have preprints which we want to remove, etc.

# We subtract the ignored publications to the inflated number of publications
data_scholar$scholar_data$Number[row.to.correct] <-
  data_scholar$scholar_data$Number[row.to.correct] -
  length(ignored.publications)

# data_scholar$scholar_data <- data_scholar$scholar_data[-7, ]

# Correct author order for specific publications (config from cv.Rmd)
if (exists("scholar_author_corrections") && length(scholar_author_corrections) > 0) {
  for (i in seq_along(scholar_author_corrections)) {
    data_scholar$scholar_publications$author <- gsub(
      names(scholar_author_corrections)[i],
      scholar_author_corrections[[i]],
      data_scholar$scholar_publications$author,
      fixed = TRUE
    )
  }
}

# Append missing author to specific publications (config from cv.Rmd)
if (exists("scholar_append_author") && length(scholar_append_author) > 0) {
  # Derive title-case author name from the lowercase scholar name
  author.name.titled <- paste(
    vapply(strsplit(author.name, " ")[[1]], function(w) {
      paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
    }, character(1)),
    collapse = " "
  )
  for (pubid in scholar_append_author) {
    if (pubid %in% data_scholar$scholar_publications$pubid) {
      idx <- which(data_scholar$scholar_publications$pubid == pubid)
      data_scholar$scholar_publications$author[idx] <- paste0(
        data_scholar$scholar_publications$author[idx],
        ", ", author.name.titled, ", et al."
      )
    }
  }
}

# Get dataframe with stats
get_stats <- function(data_scholar, author.name = author.name) {
  # Counts are based on the filtered publication list.
  # Author position
  authors <- tolower(data_scholar$scholar_publications$author)
  authors <- strsplit(authors, ", ")
  position <- sapply(authors, function(x) {
    # Exact match first
    pos <- which(x == author.name)
    # Fallback: partial match (handles name format changes from Google Scholar)
    if (length(pos) == 0) {
      pos <- grep(author.name, x, fixed = TRUE)
    }
    # Still no match: skip this publication
    if (length(pos) == 0) {
      return("Other")
    }
    pos <- pos[1]
    if (pos == 1) {
      return("First")
    }
    if (pos == 2) {
      return("Second")
    }
    if (pos == length(x)) {
      return("Last")
    }
    "Other"
  })

  position <- as.data.frame(t(as.matrix(table(position))))
  # Manual corrections --------------------
  # Nicolas & Makowski 2016
  #position$Last <- position$Last - 1 #
  #position$Second <- position$Second + 1
  # Sperduti & Makowski 2017 (fiction 2)
  #position$First <- position$First + 1 #
  #position$Second <- position$Second - 1

  # Stats
  data.frame(
    "n.Publications" = length(authors),
    "n.FirstAuthor" = ifelse(is.null(position$First), 0, position$First),
    "n.SecondAuthor" = ifelse(is.null(position$Second), 0, position$Second),
    "n.LastAuthor" = ifelse(is.null(position$Last), 0, position$Last),
    # h-index and total citations come from Google Scholar profile totals,
    # and therefore are never altered by `scholar_exclude`.
    "H.index" = data_scholar$scholar_stats$h_index,
    "Citations" = data_scholar$scholar_stats$total_cites
  )
}


# Make plot with number of publications and number of citations
plot_impact <- function(data_scholar) {
  data <- data_scholar$scholar_data
  if (is.null(data) || nrow(data) == 0) {
    message("No scholar data available for plotting.")
    return(invisible(NULL))
  }

  data %>%
    dplyr::filter(Year >= 2010) %>%
    ggplot(aes(x = Year, y = Number)) +
    geom_bar(aes(alpha = Year), stat = "identity") +
    geom_line(aes(colour = Index), linewidth = 2) +
    see::theme_modern() +
    ylab("") +
    scale_x_continuous(labels = as.character(data$Year), breaks = data$Year) +
    scale_color_manual(values = c("#2196F3", "#E91E63")) +
    facet_wrap(~Index, scales = "free", strip.position = "top") +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(face = "plain", size = 16),
      axis.title = element_text(face = "plain", size = 16),
      axis.title.x = element_blank(),
      legend.position = "none"
    )
}


# Make plot with number of publications and number of citations
plot_citations_per_paper <- function(data_scholar) {
  data_scholar[["scholar_publications"]] %>%
    ggplot(aes(x = Publication, y = cites, label = Journal)) +
    geom_bar(aes(fill = Publication), stat = "identity") +
    see::theme_modern() +
    scale_fill_material_d(palette = "rainbow", reverse = TRUE) +
    ylab("Number of citations") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

# Define table_impact function
table_impact <- function(
  data_scholar,
  author.name = author.name,
  scholar.profile = NULL,
  language = "EN",
  font_size = 9
) {
  if (
    is.null(data_scholar$scholar_publications) ||
      nrow(data_scholar$scholar_publications) == 0
  ) {
    message("No scholar publications available for table.")
    return(invisible(NULL))
  }

  gs_profile <- paste0(
    "https://scholar.google.com/citations?user=",
    scholar.profile
  )
  # gs_profile_URL <- paste0("\\href{", gs_profile, "}{Google Scholar Profile}")
  gs_profile_URL <- link("Google Scholar Profile", gs_profile, color = NULL)
  # gs_profile_URL <- paste0("Google Scholar Profile: ", gs_profile)
  if (language == "EN") {
    caption <- "Table automatically updated through my"
  } else if (language == "FR") {
    caption <- "Tableau mis à jour automatiquement via mon"
  }

  get_stats(data_scholar = data_scholar, author.name = author.name) |>
    knitr::kable(
      col.names = c(
        "\\textit{n}-Publications* \\textit{(total)}",
        "\\textit{n}-1\\textsuperscript{st} author",
        "\\textit{n}-2\\textsuperscript{nd} author",
        "\\textit{n}-Senior author",
        "H-index",
        "Citations \\textit{(total)}"
      ),
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      align = "c"
    ) %>%
    kableExtra::kable_styling(
      position = "center",
      font_size = font_size,
      latex_options = "hold_position"
    ) %>%
    kableExtra::add_footnote(
      paste(caption, gs_profile_URL),
      notation = "symbol",
      escape = FALSE
    )
}
