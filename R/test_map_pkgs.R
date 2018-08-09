
# Work with sf ------------------------------------------------------------

pacman::p_load(tidyverse, sf)

system.file("gpkg/nc.gpkg", package = "sf")

nc <- read_sf(system.file("gpkg/nc.gpkg", package = "sf"))
nc %>% glimpse()

nc2 <-
  nc %>%
  st_transform(32119) %>%
  select(SID74, SID79, geom) %>%
  gather(VAR, SID, -geom)

ggplot() +
  geom_sf(data = nc2, aes(fill = SID)) +
  facet_wrap(~ VAR, ncol = 1)


# Work with urbnmapr ------------------------------------------------------

# devtools::install_github("UrbanInstitute/urbnmapr")
pacman::p_load(tidyverse, urbnmapr, urban_R_theme)

# Basic USA
states %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray", color = "white") +
  coord_map("albers", lat0 = 39, lat1 = 45)


# Household Data
countydata
counties
household_data <- left_join(countydata, counties, by = "county_fips")

household_data %>%
  ggplot(aes(long, lat, group = group, fill = medhhincome)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Median Household Income")


countydata %>%
  left_join(counties, by = "county_fips") %>%
  filter(state_name == "Michigan") %>%
  ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = "#ffffff", size = .25) +
  # scale_fill_gradientn(labels = scales::percent,
  #                      guide = guide_colorbar(title.position = "top")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Homeownership Rate") +
  theme_minimal()



# Test Case ---------------------------------------------------------------

library(tidyverse)
library(urbnmapr)


mi_elections <-
  read_rds("data/election_mi.rds") %>%
  filter(ElectionDate >= "2018-01-01") %>%
  mutate(CountyName = str_replace(CountyName, "GD. TRAVERSE", "GRAND TRAVERSE"))


counties %>% glimpse()
mi_elections %>% glimpse()


mi_election_dem_gov_county <-
  mi_elections %>%
  filter(PartyName == "DEM",
         `OfficeCode(Text)` == "02") %>%
  group_by(CountyName, CandidateLastName) %>%
  summarise(Votes = sum(CandidateVotes)) %>%
  group_by(CountyName) %>%
  mutate(Percent = Votes / sum(Votes))


match_county <-
  counties %>%
  mutate(county_match = county_name %>% str_to_upper() %>% str_remove_all(" COUNTY")) %>%
  filter(state_abbv == "MI")


mi_election_map <-
  left_join(mi_election_dem_gov_county,
            match_county,
            by = c("CountyName" = "county_match"))


mi_election_map %>%
  ggplot(aes(long, lat, group = group, fill = Percent)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  theme_minimal() +
  facet_wrap(~ CandidateLastName)
