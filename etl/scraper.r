library(tidyverse)
library(rvest)
library(janitor)

years <- c(1924, 1928, 1932, 1936, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988, 1992, 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022)

tibble <- as.tibble(years) |>
  mutate_all(as.character)

tibble <- cbind(tibble,
  url_prefix = "https://en.wikipedia.org/wiki/",
  url_suffix = "_Winter_Olympics_medal_table"
) |> mutate(tibble, url = str_c(url_prefix, value, url_suffix))

fetch_medal_table <- function(url) {
  tryCatch(
    {
      page <- read_html(url)
      table <- page |>
        html_elements("table.wikitable") |>
        html_table() 

      medal_table <- table[[1]] |> mutate(across(where(is.integer), as.character))

      return(medal_table)

    },
    error = function(cond) {
      message("Error scraping table: ", url, cond$message)
    }
  )
}

medal_table <- tibble |>
  mutate(
    table_data = map(url, fetch_medal_table)
  ) |> 
  unnest(table_data)

# Merge the nations and NOC columns together
medal_table_clean <- medal_table |> 
  mutate(nation = coalesce(Nation, NOC)) |> 
  mutate(nation = coalesce(nation, `.mw-parser-output .tooltip-dotted{border-bottom:1px dotted;cursor:help}NOC`)) |> 
  select(-c(url, url_suffix, url_prefix, Nation, NOC, Rank, `.mw-parser-output .tooltip-dotted{border-bottom:1px dotted;cursor:help}NOC`)) |> 
  filter(!str_detect(nation, "^Total(s)?")) |> 
  rename(year = value) |> 
  clean_names()

medal_table_clean <- medal_table_clean |> 
  mutate(nation = str_remove(nation, "\\*+$")) |> 
  mutate(nation = str_remove(nation, "\\s*\\([^)]*\\)$")) |> 
  mutate(nation = str_remove(nation, "\\s*\\[.*?\\]$")) |> 
  mutate(nation = str_remove(nation, "\\‡+$")) |> 
  mutate(nation = str_extract(nation, "^[^*]+"))

write_csv(medal_table_clean, "data/medal_table_clean.csv")
