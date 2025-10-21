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
  lubridate,
  tigris,
  sf,
  gganimate,
  gifsk,
  plotly
)


arrests <- arrow::read_parquet("data/arrest_data.parquet") |> 
  clean_names()

licenses <- arrow::read_parquet("data/licenses.parquet") |>
  clean_names()

shooting <- arrow::read_parquet("data/shooting.parquet") |>
  clean_names()

arrests |> 
  distinct(pd_desc) |> 
  pull()
arrests |> 
  glimpse()
licenses |> 
  glimpse()
shooting |> 
  glimpse()

nyc_boroughs <- counties(state = "NY", cb = TRUE)|> 
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond"))



crime_category <- function(crime) {
  case_when(
    str_detect(crime, regex("ASSAULT|ROBBERY|HOMICIDE|RAPE|SEXUAL|KIDNAPPING|ENDANGERMENT|MENACING", ignore_case=TRUE)) ~ "Violent Crime",
    str_detect(crime, regex("BURGLARY|LARCENY|THEFT|FORGERY|FRAUD|ARSON|MISCHIEF|GRAFFITI", ignore_case=TRUE)) ~ "Property Crime",
    str_detect(crime, regex("CONTROLLED|DRUG|CANNABIS|ALCOHOL|IMPAIRED", ignore_case=TRUE)) ~ "Drug/Alcohol",
    str_detect(crime, regex("WEAPON|FIREWORKS|EXPLOSIVE", ignore_case=TRUE)) ~ "Weapons/Explosives",
    str_detect(crime, regex("TRESPASS|DISORDERLY|LOITERING|TRAFFIC|BRIBERY|PUBLIC|CODE|HEALTH", ignore_case=TRUE)) ~ "Public Order/Admin",
    TRUE ~ "Other"
  )
}

arrests <- arrests |> 
  mutate( category = crime_category(pd_desc)) 

arrests |> 
  filter(category == "Other") |> 
  pull(pd_desc)

arrests_loc_top_200 <- arrests |> 
  select(latitude, longitude, category) |> 
  drop_na(latitude, longitude) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(st_crs(nyc_boroughs)) |> 
  summarise(
    .by = c(geometry, category),
    n = n()
  ) |> 
  slice_max(n, n = 1000)



ggplot() +
  geom_sf(data = nyc_boroughs, fill = "lightgray", color = "black") +
  geom_sf(data = arrests_loc_top_200, aes(color = category), alpha = 0.5, size = 0.2) +
  coord_sf(
    xlim = c(-74.3, -73.7),
    ylim = c(40.5, 40.95),
    expand = FALSE
  ) +
  theme_minimal() +
  labs(title = "NYC Taxi Pickups")



p <- ggplot() +
  geom_sf(data = nyc_boroughs, fill = "lightgray", color = "black") +
  geom_sf(data = arrests_loc, aes(color = year), alpha = 0.6, size = 2) +
  coord_sf(
    xlim = c(-74.3, -73.7),
    ylim = c(40.5, 40.95),
    expand = FALSE
  ) +
  labs(title = "NYC Arrests: {closest_state}") +
  theme_minimal() +
  transition_states(year, transition_length = 2, state_length = 1) +
  ease_aes('linear')

animate(p, nframes = 100, fps = 10)

p <- ggplot(arrests_loc) +
  geom_sf(data = nyc_boroughs, fill = "lightgray", color = "black") +
  geom_sf(aes(color = year), size = 2, alpha = 0.6) +
  coord_sf(xlim = c(-74.3, -73.7), ylim = c(40.5, 40.95), expand = FALSE) +
  theme_minimal()


  )


