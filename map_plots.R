
# create plots to load into shiny app -------------------------------------



# packages and data ---------------------------------------------------------------

# packages
library(tidyverse)
library(maps)
library(viridis)

# read in dataset
imm_dat <- read_rds("data/processed/imm_dat_clean.rds")
# lat/long data from map_data
states <- map_data("state") %>%
  rename(state = region)


# datasets for plots ------------------------------------------------------

# join datasets based on Percentage of bills enacted
imm_dat_map <- imm_dat %>%
  mutate(state = tolower(state)) %>%
  mutate(enacted = ifelse(enacted == "Yes", 1, 0)) %>%
  group_by(state) %>%
  summarize(n = n(),
            passed = sum(enacted),
            prop = passed/n) %>%
  mutate(enacted = cut(prop,
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                       right = FALSE)) %>%
  left_join(states, by = "state")


# join datasets based on whether bills targets undocumented individuals
imm_dat_map_undoc <- imm_dat %>%
  mutate(state = tolower(state)) %>%
  mutate(targets_undocumented = ifelse(targets_undocumented == "Yes", 1, 0)) %>%
  group_by(state) %>%
  summarize(
    n = n(),
    passed = sum(targets_undocumented),
    prop = passed/n) %>% 
  mutate(targets_undocumented = cut(prop,
                                    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                    labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                                    right = FALSE)) %>%
  left_join(states, by = "state")

# join datasets based on whether bills targets undocumented children
imm_dat_map_undoc_child <- imm_dat %>%
  mutate(state = tolower(state)) %>%
  mutate(targets_undocumented_children = ifelse(targets_undocumented_children == "Yes", 1, 0)) %>%
  group_by(state) %>%
  summarize(
    n = n(),
    passed = sum(targets_undocumented_children),
    prop = passed/n) 
  mutate(targets_undocumented_children = cut(prop,
                                             breaks = c(0, 0.05, 0.10, 0.15, 0.20, 0.25),
                                             labels = c("0-5%", "5-10%", "10-15%", "15-20%", "20-25%"),
                                             right = FALSE)) %>%
  left_join(states, by = "state")


# ggplot maps -------------------------------------------------------------

# overall map ----
overall_enacted <- ggplot(imm_dat_map, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = enacted), color = "grey80") +
    scale_fill_viridis(discrete = TRUE) +
    coord_map() +
    labs(
      title = "Percentage of Immigration Bills Enacted Across the US\n",
      fill = "Percentage of\nbills enacted:") +
    theme_void() +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
    )

# # save the map
# ggsave("www/overall_enacted.png", overall_enacted, width = 7, height = 4.5, units = "in", dpi = "print")



# undocumented individuals ----
undoc_indiv <- ggplot(imm_dat_map_undoc, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = targets_undocumented), color = "grey80") +
    scale_fill_viridis(discrete = TRUE) +
    coord_map() +
    labs(
      title = "Percentage of Immigration Bills Targeting\nUndocumented Individuals Across the US\n",
      fill = "Percentage of bills targeting\nundocumented individuals:") +
    theme_void() +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
    )


# # save the map
# ggsave("www/undoc_indiv.png", undoc_indiv, width = 7, height = 4.5, units = "in", dpi = "print")



# undocumented children ----
undoc_children <- ggplot(imm_dat_map_undoc_child, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = targets_undocumented_children), color = "grey80") +
    scale_fill_viridis(discrete = TRUE) +
    coord_map() +
    labs(
      title = "Percentage of Immigration Bills Targeting\nUndocumented Children Across the US\n",
      fill = "Percentage of bills targeting\nundocumented children:") +
    theme_void() +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
    )

# # save the map
# ggsave("www/undoc_children.png", undoc_children, width = 7, height = 4.5, units = "in", dpi = "print")
