
# Gather Data -------------------------------------------------------------

# https://www.michigan.gov/sos/0,4670,7-127-1633_8722---,00.html

pacman::p_load(tidyverse, readr, foreach, rlang)


filename_all <- dir(path = "data/tsv/")
filename_one <- filename_all %>% last()

# test_one <-
#   str_glue("data/tsv/{filename_one}") %>%
#   read_tsv() %>%
#   filter(!is.na(ElectionDate) & !is.na(CandidateID)) %>%
#   set_tidy_names()


import_all <-
  foreach(i = seq_along(filename_all)) %do% {

    lst(!!sym(filename_all[[i]]) :=
          str_glue("data/tsv/{filename_all[[i]]}") %>%
          read_tsv(col_types = cols(
            ElectionDate = col_date(format = ""),
            `OfficeCode(Text)` = col_character(),
            `DistrictCode(Text)` = col_character(),
            StatusCode = col_integer(),
            CountyCode = col_integer(),
            CountyName = col_character(),
            OfficeDescription = col_character(),
            PartyOrder = col_character(),
            PartyName = col_character(),
            PartyDescription = col_character(),
            CandidateID = col_integer(),
            CandidateLastName = col_character(),
            CandidateFirstName = col_character(),
            CandidateMiddleName = col_character(),
            CandidateFormerName = col_character(),
            CandidateVotes = col_integer(),
            `WriteIn(W)/Uncommitted(Z)` = col_character(),
            `Recount(*)` = col_character(),
            `Nominated(N)/Elected(E)` = col_character()
          )) %>%
          filter(!is.na(ElectionDate) & !is.na(CandidateID)) %>%
          set_tidy_names()
    )


  }

import_tibble <-
  import_all %>%
  flatten() %>%
  enframe()

import_all %>%
  flatten() %>%
  map(keep, lubridate::is.Date)

test_names <-
  import_tibble %>%
  transmute(colnames = map(value, colnames))

test_names %>%
  unnest(colnames) %>%
  count(colnames)


single_table <-
  import_all %>%
  flatten() %>%
  bind_rows(.id = "election_year") %>%
  mutate(ElectionType = case_when(
    str_detect(election_year, "GEN") ~ "General",
    str_detect(election_year, "PRI") ~ "Primary",
    str_detect(election_year, "PPR") ~ "Presidential Primary"
  ))


write_csv(single_table, "data/election_mi.csv")
write_rds(single_table, "data/election_mi.rds", compress = "gz")
