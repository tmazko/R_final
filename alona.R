if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  rvest,
  janitor,
  ggrepel,
  jsonlite,
  httr2,
  treemapify,
  plotly,
  viridis,
  lubridate
)


arrests <- arrow::read_parquet("data/arrest_data.parquet") |> 
  clean_names()

licenses <- arrow::read_parquet("data/licenses.parquet") |>
  clean_names()


arrests |> 
  glimpse()
