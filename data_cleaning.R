
# Data Visualization: STAT 302 - final project 2021/02/20 -----------------


# load packages -----------------------------------------------------------

library(tidyverse)
library(skimr)
library(viridis)

# read data ---------------------------------------------------------------

imm_dat_test <- readRDS("data/unprocessed/immigration_legislation_data.RDS") %>% 
  clean_names()

# lat/long data from map_data
states <- map_data("state") %>% 
  rename(state = region) 


# check data --------------------------------------------------------------

skim_without_charts(imm_dat_test)


# clean data --------------------------------------------------------------

imm_dat_clean <- imm_dat_test %>% 
  mutate(
    state = factor(state,
                   levels = 1:51,
                   labels = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                              "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
                              "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
                              "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                              "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                              "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                              "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                              "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                              "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                              "Virginia", "Washington", "Washington, DC", "West Virginia", "Wisconsin",
                              "Wyoming"
                   )
                   ),
    year_introduced = factor(year_introduced,
                             levels = c(1:27),
                             labels = c("1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", 
                                        "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                                        "2010", "2011", "2012", "2013", "2014", "2015", "2016")
                             ),
    bill_type = factor(bill_type,
                       levels = c("1", "2", "3", "4", "5", "9", "11", "13", "6"),
                       labels = c("Statute", "Resolution", "Memorial", "Initiative/Referendum", "Appropriation",
                                  "Constitutional amendment", "Private bill", "Executive order", "Other")
                       ),
    final_status = factor(final_status,
                          levels = c("13", "6", "1", "7", "3", "4", "23", "9", "2", "5", "8"),
                          labels = c("Active", "Died in committee", "Enacted", "Introduced", "Passed House only", 
                                     "Passed Senate only", "Passed both chambers", "Substituted", 
                                     "Vetoed", "Withdrawn", "Other")
                          ),
    targets_undocumented = factor(targets_undocumented,
                                  levels = c("1", "0"),
                                  labels = c("Yes", "No")
                                  ),
    targets_undocumented_children = factor(targets_undocumented_children,
                                           levels = c("1", "0"),
                                           labels = c("Yes", "No")
                                           ),
    targets_legal_permanent_residents = factor(targets_legal_permanent_residents,
                                               levels = c("1", "0"),
                                               labels = c("Yes", "No")
                                               ),
    targets_refugees_asylees = factor(targets_refugees_asylees,
                                      levels = c("1", "0"),
                                      labels = c("Yes", "No")
                                      ),
    enacted = factor(enacted,
                     levels = c("FALSE", "TRUE"),
                     labels = c("No", "Yes"))
    )

# factor groupings
imm_dat_fct <- imm_dat_clean %>% 
  mutate(decade = fct_collapse(year_introduced,
                               "1990s" = c("1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999"),
                               "2000s" = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009"),
                               "2010s" = c("2010", "2011", "2012", "2013", "2014", "2015", "2016")
                               ),
  division = fct_collapse(state,
                          "New England" = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont"),
                          "Mid-Atlantic" = c("New Jersey", "New York", "Pennsylvania"),
                          "East North Central" = c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin"),
                          "West North Central" = c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"),
                          "South Atlantic" = c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", 
                                               "Virginia", "Washington, DC", "West Virginia"),
                          "East South Central" = c("Alabama", "Kentucky", "Mississippi", "Tennessee"),
                          "West South Central" = c("Arkansas", "Louisiana", "Oklahoma", "Texas"),
                          "Mountain" = c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming"),
                          "Pacific" = c("Alaska", "California", "Hawaii", "Oregon", "Washington")
                          ),
  region = fct_collapse(division,
                        "Northeast" = c("New England", "Mid-Atlantic"),
                        "Midwest" = c("East North Central", "West North Central"),
                        "South" = c("South Atlantic", "East South Central", "West South Central"),
                        "West" = c("Mountain", "Pacific")
                        )
  ) %>% 
  select(id, state, division, region, year_introduced, decade, everything())

# # save out immigration dataset
# imm_dat_fct %>%
# write_rds(file = "data/processed/imm_dat_fct.rds")


