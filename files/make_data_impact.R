# ------------------------------
# Scholar data
# ------------------------------

data_scholar <- list()

data_scholar[["date"]] <- format(Sys.time(), "%d %B %Y")
data_scholar[["scholar_stats"]] <- scholar::get_profile(scholar.profile)
data_scholar[["scholar_history"]] <- scholar::get_citation_history(scholar.profile)
capture.output(data_scholar[["scholar_publications"]] <-  scholar::get_publications(scholar.profile, 
                                                                                    flush = TRUE) |>
                 mutate(author = scholar::get_complete_authors(scholar.profile, pubid)))

# Correct author name inconsistencies, if necessary! (uncomment below)
data_scholar$scholar_publications$author <- gsub("R Thériault", "Rém Thériault", data_scholar$scholar_publications$author)
# data_scholar$scholar_publications$author <- gsub("MM Doucerain |MM. Doucerain", "M Doucerain", data_scholar$scholar_publications$author)
#                                                  data_scholar$scholar_publications$author)

data_scholar[["scholar_data"]] <- data_scholar[["scholar_publications"]]  %>%
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
  rbind(data_scholar[["scholar_history"]] %>%
          dplyr::rename(Number = cites,
                        Year = year) %>%
          mutate(Index = "Citations",
                 Number = cumsum(Number)))

# Publications individual
data <- data_scholar[["scholar_publications"]] %>%
  mutate(Authors = paste0(lapply(stringr::str_split(author, " "), `[[`, 2)),
         Authors = stringr::str_to_title(stringr::str_remove_all(Authors, ",")),
         Publication = paste0(Authors, " (", year),
         Journal = stringr::str_to_title(journal))


# Disambiguate unique
suffix <- letters
suffix[1] <- ""
for(i in 1:nrow(data)){
  pub <- data$Publication[i]
  j <- 1
  while(paste0(pub, suffix[j]) %in% data$Publication[1:i-1]){
    j <- j + 1
  }
  data$Publication[i] <- paste0(pub, suffix[j])
}

# Ignore the publications below!
ignored.publications <- c("Thériault (2022)", 
                          "Thériault (2022c)",
                          "Thériault (2022d)",
                          "Makowski (2023)",
                          "Lüdecke (2023)")

# Filter some publications out, as desired
data_scholar[["scholar_publications"]] <- data %>%
  mutate(Publication = paste0(Publication, ")"),
         Publication = fct_reorder(Publication, cites, .desc = TRUE)) %>%
  filter(!Publication %in% ignored.publications)

# Manual correction for non-publications
# (You might need to change this yourself!)
row.to.correct <- nrow(data_scholar[["scholar_publications"]])

data_scholar$scholar_data$Number[row.to.correct] <- 
  data_scholar$scholar_data$Number[row.to.correct] - length(ignored.publications)

data_scholar$scholar_data <- data_scholar$scholar_data[-7, ]

# Correct publication with several first authors
if(any(grepl("M Miglianico, Rém Thériault", data_scholar$scholar_publications$author))) {
  data_scholar$scholar_publications$author <- gsub("M Miglianico, Rém Thériault", "Rém Thériault, M Miglianico",
       data_scholar$scholar_publications$author)
}

# Get dataframe with stats
get_stats <- function(data_scholar, author.name = author.name) {
  
  # Author position
  authors <- tolower(data_scholar$scholar_publications$author)
  authors <- strsplit(authors, ", ")
  position <- sapply(authors, function(x) {
    position <- which(x == author.name)
    if(position == 1) return("First")
    if(position == 2) return("Second")
    if(position == length(x)) return("Last")
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
    "n.FirstAuthor" = ifelse(is.null(position$First),
                             0, position$First),
    "n.SecondAuthor" = ifelse(is.null(position$Second),
                              0, position$Second),
    "n.LastAuthor" = ifelse(is.null(position$Last),
                            0, position$Last),
    "H.index" = data_scholar$scholar_stats$h_index,
    "Citations" = data_scholar$scholar_stats$total_cites
  )
}


# Make plot with number of publications and number of citations
plot_impact <- function(data_scholar) {
  data <- data_scholar$scholar_data
  
  data %>%
    dplyr::filter(Year >= 2010) %>%
    ggplot(aes(x = Year, y = Number)) +
    geom_bar(aes(alpha=Year), stat="identity") +
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
  data_scholar[["scholar_publications"]]  %>%
    ggplot(aes(x = Publication, y = cites, label = Journal)) +
    geom_bar(aes(fill=Publication), stat="identity") +
    see::theme_modern() +
    scale_fill_material_d(palette="rainbow", reverse=TRUE) +
    ylab("Number of citations") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle=45, hjust=1),
      legend.position = "none"
    )
}

# Define table_impact function
table_impact <- function(data_scholar, author.name = author.name){
  get_stats(data_scholar = data_scholar, author.name = author.name) |>
  knitr::kable(
    col.names = c(
      "\\textit{n}-Publications \\textit{(total)}",
      "\\textit{n}-1\\textsuperscript{st} author",
      "\\textit{n}-2\\textsuperscript{nd} author",
      "\\textit{n}-Senior author",
      "H-index",
      "Citations \\textit{(total)}"
    ),
    format = "latex",
    booktabs = TRUE,
    escape = FALSE
  ) %>%
  kableExtra::kable_styling(position = "center", font_size = 8, latex_options = "hold_position")
}
