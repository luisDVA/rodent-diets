# MammalDIET 1&2 Exploratory Data Analysis

# Load packages -----------------------------------------------------------
library(dplyr) # [CRAN]
library(readr) # [github::tidyverse/readr]
library(readxl) # [CRAN]
library(janitor) # [CRAN]
library(tidyr) # [CRAN]
library(ggplot2) # [CRAN]
library(treemapify) # [CRAN]
library(hrbrthemes) # [github::hrbrmstr/hrbrthemes]
library(waffle) # [github::hrbrmstr/waffle]
library(extrafont) # [CRAN]
library(cowplot) # [CRAN]

# Load data ---------------------------------------------------------------
# MammalDIET, from Kissling et al. 2014 - https://datadryad.org/stash/dataset/doi:10.5061/dryad.6cd0v
Mdiet <- read_tsv("data/MammalDIET_v1.0.txt")
# MammalDIET2 from Gainsbury et al. 2018 - https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fmam.12119&file=mam12119-sup-0001-AppendixS1.xlsx
gains18 <- read_xlsx("data/Gainsbury-mam12119-sup-0001-appendixs1.xlsx", skip = 2, sheet = 1)

# Data preprocess ---------------------------------------------------------
# Clean data before joining
# MammaDIET, no extrapolated data
Mdietfilt <- Mdiet %>%
  clean_names() %>%
  mutate(binomial = paste(genus, species), source = "mammalDIET") %>%
  filter(data_source != "Extrapolated")
# Label species from MammalDIET2
gains18 <- gains18 %>%
  clean_names() %>%
  mutate(source = "Gainsbury2018")
# Bind into a single object
alldiet <- bind_rows(Mdietfilt, gains18)
# Keep rodents
rods <- alldiet %>% filter(order == "RODENTIA")
# How many rodents with no extrapolated the data?
rods %>% nrow
# Tally trophic levels
rodTroph <- rods %>% count(trophic_level)

# Feeding guilds
rodguilds <- rods %>% select(family, binomial, mammal_eater:folivore)
# Keep only species with assigned guilds
rodguilds <- rodguilds %>% filter_at(vars(mammal_eater:folivore), any_vars(. == 1))
# For tallying
rodguildsCount <- rodguilds %>%
  gather(guild, value, mammal_eater:folivore) %>%
  group_by(guild) %>%
  summarise(guild_n = sum(value, na.rm = TRUE)) %>%
  ungroup()
# Multiple guilds per species
rodguildMemberships <- rodguilds %>%
  gather(guild, value, mammal_eater:folivore) %>%
  filter(value != 0) %>%
  group_by(family, binomial) %>%
  summarise(nguilds = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  add_count(family)
rodguildMemberships <- rodguildMemberships %>%
  group_by(family) %>%
  mutate(pct1 = length(which(nguilds == 1)) / n)
# Report mean guilds
mean(rodguildMemberships$nguilds)


# Plot trophic levels -----------------------------------------------------
# font choice is optional
levelsPtm <-
  rodTroph %>%
  mutate(vals = round(n / 1198 * 100, 0)) %>%
  ggplot() +
  geom_treemap(aes(area = vals, fill = trophic_level), colour = "black") +
  geom_treemap_text(aes(area = vals, label = n, family = "Quattrocento Sans"),
    place = "bottomright", colour = "white"
  ) +
  theme_ipsum(grid = "", base_size = 15) +
  theme_enhance_waffle() +
  labs(tag = "a)") +
  theme(legend.position = "bottom", text = element_text(family = "Quattrocento Sans")) +
  scale_fill_manual(
    name = "Trophic Level",
    values = c("#ffa100", "#C6C16D", "#628CA5")
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 3))
levelsPtm

# Plot feeding guilds -----------------------------------------------------
# font choice is optional
guildsLong <- rodguildsCount %>%
  mutate(vals = round(guild_n / 2029 * 100, 0)) %>%
  rename(names = 1) %>%
  mutate(names = stringr::str_replace(names, "_", " ")) %>%
  mutate(names = stringr::str_to_title(names))
guildsLong[5, 3] <- 4

guildsPtm <-
  ggplot(guildsLong, aes(fill = names, area = vals)) +
  geom_treemap(colour = "black") +
  geom_treemap_text(aes(label = guild_n, family = "Quattrocento Sans"),
    place = "bottomright", colour = "white"
  ) +
  theme_ipsum(grid = "", base_size = 15) +
  theme_enhance_waffle() +
  labs(tag = "b)") +
  theme(legend.position = "bottom", text = element_text(family = "Quattrocento Sans")) +
  scale_fill_manual(
    name = "Feeding Guild",
    values = c("#384a22", "#5A5F9D", "#dca57e", "#514F5C", "#E38A22")
  ) +
  guides(fill = guide_legend(title.position = "top", nrow = 3))
guildsPtm

# Make multipanel figure --------------------------------------------------
plot_grid(levelsPtm, guildsPtm, align = "h")
