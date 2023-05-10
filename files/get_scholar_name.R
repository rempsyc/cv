# ------------------------------
# Scholar data
# ------------------------------
library(dplyr)
library(scholar)

scholar::get_publications(scholar.profile, 
                          flush = TRUE) %>% 
  mutate(author = scholar::get_complete_authors(scholar.profile, pubid)) %>% 
  select(author) %>% 
  print
