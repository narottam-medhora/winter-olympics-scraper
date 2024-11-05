library(tidyverse)
library(jsonlite)
library(countrycode)

tibble <- read_csv("data/medal_table_clean.csv")

# Add the continent
tibble <- tibble |> 
  mutate(continent = countrycode(sourcevar = tibble$nation, origin="country.name", destination = "continent"))

# Bucket the NAs to Other
tibble <- tibble |> 
  mutate(continent = replace_na(continent, "Other"))

# Convert the columns into appropriate data types
tibble$year <- as.factor(as.character(tibble$year))
tibble$continent <- as.factor(as.character(tibble$continent))
tibble$nation <- as.factor(as.character(tibble$nation))
tibble$total <- as.numeric(tibble$total)

# Group the data by continent
tibble <- tibble |> 
  group_by(year, continent) |> 
  summarize(total=sum(total))

# Write the data
write_csv(tibble, "data/medals_by_continent.csv")
