library(rvest)
library(polite)
library(tidyverse)
library(janitor)
library(httr)

url <- "https://en.wikipedia.org/wiki/United_States_at_the_Winter_Olympics"
url_bow <- polite::bow(url)
url_bow

ind_html <- 
  polite::scrape(url_bow) |> 
  rvest::html_nodes("table.wikitable") |> 
  rvest::html_table(fill = TRUE)

ind_table <- 
  ind_html[[3]] |> 
  clean_names()

ind_table <- ind_table |> filter(row_number() <= n() - 1)

# Plot the data
us_medals <- ggplot(ind_table, aes(x=sport, y=total)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggsave('united_states.png', us_medals, NULL, "./viz")
