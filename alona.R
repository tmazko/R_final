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



# crime_category <- function(crime) {
#   case_when(
#     str_detect(crime, regex("ASSAULT|ROBBERY|HOMICIDE|RAPE|SEXUAL|KIDNAPPING|ENDANGERMENT|MENACING", ignore_case=TRUE)) ~ "Violent Crime",
#     str_detect(crime, regex("BURGLARY|LARCENY|THEFT|FORGERY|FRAUD|ARSON|MISCHIEF|GRAFFITI", ignore_case=TRUE)) ~ "Property Crime",
#     str_detect(crime, regex("CONTROLLED|DRUG|CANNABIS|ALCOHOL|IMPAIRED", ignore_case=TRUE)) ~ "Drug/Alcohol",
#     str_detect(crime, regex("WEAPON|FIREWORKS|EXPLOSIVE", ignore_case=TRUE)) ~ "Weapons/Explosives",
#     str_detect(crime, regex("TRESPASS|DISORDERLY|LOITERING|TRAFFIC|BRIBERY|PUBLIC|CODE|HEALTH", ignore_case=TRUE)) ~ "Public Order/Admin",
#     TRUE ~ "Other"
#   )
# }

crime_category <- function(crime) {
  case_when(
    str_detect(crime, regex("ASSAULT|ROBBERY|HOMICIDE|RAPE|SEXUAL|KIDNAPPING|ENDANGERMENT|MENACING|STRANGULATION|OBSTR BREATH/CIRCUL|AGGRAVATED HARASSMENT|MAKING TERRORISTIC THREAT|FORCIBLE TOUCHING|MURDER|COERCION|SODOMY|STALKING", ignore_case=TRUE)) ~ "Violent Crime",
    str_detect(crime, regex("BURGLARY|LARCENY|THEFT|FORGERY|FRAUD|ARSON|MISCHIEF|GRAFFITI|STOLEN PROPERTY|UNAUTHORIZED USE VEHICLE|BURGLARS TOOLS", ignore_case=TRUE)) ~ "Property Crime",
    str_detect(crime, regex("CONTROLLED|DRUG|CANNABIS|ALCOHOL|IMPAIRED|MARIJUANA|POSS METH|SYNTHETIC", ignore_case=TRUE)) ~ "Drug/Alcohol",
    str_detect(crime, regex("WEAPON|FIREWORKS|EXPLOSIVE|CRIM POS WEAP|FIREARMS", ignore_case=TRUE)) ~ "Weapons/Explosives",
    str_detect(crime, regex("TRESPASS|DISORDERLY|LOITERING|TRAFFIC|BRIBERY|PUBLIC|CODE|HEALTH|BAIL JUMPING|ESCAPE|IMPRISONMENT|CONTEMPT|NUISANCE|OBSCENITY|FUGITIVE|UNLICENSED OPERATOR|RADIO DEVICES|APPEARANCE TICKET|GENERAL BUSINESS LAW|NY STATE LAWS|NYS|NYC UNCLASSIFIED", ignore_case=TRUE)) ~ "Public Order/Admin",
    str_detect(crime, regex("PROSTITUTION|PATRONIZING|PROMOTING|LICENSED PREMISES|COMPULSORY|SEX CRIMES|INCEST|LURING A CHILD", ignore_case=TRUE)) ~ "Sex Crimes",
    str_detect(crime, regex("ANIMAL|TORTURE/INJURE|NEGLECT/POISON|CONFINING ANIMAL|CAUSE SPI/KILL|ABANDON ANIMAL", ignore_case=TRUE)) ~ "Animal Crimes",
    str_detect(crime, regex("RECKLESS DRIVING|SPEEDING|FAIL TO SIGNAL|SEAT BELTS|IMPROPER LIGHTS|ONE WAY STREET", ignore_case=TRUE)) ~ "Traffic",
    str_detect(crime, regex("MONEY LAUNDERING|SALE|SALES OF PRESCRIPTION|UNAUTH. SALE", ignore_case=TRUE)) ~ "Financial/White Collar",
    TRUE ~ "Other"
  )
}

arrests <- arrests |> 
  mutate( category = crime_category(pd_desc)) 

arrests |> 
  summarise(
    .by = category,
    n = n()
  )

arrests |> 
  filter(category == "Other") |> 
  distinct(pd_desc) |> 
  pull()

arrests_loc_top_200 <- arrests |> 
  select(latitude, longitude, category) |> 
  drop_na(latitude, longitude) |> 
  filter(latitude != 0, longitude != 0,
         category != "Other") |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
  st_transform(st_crs(nyc_boroughs)) |> 
  summarise(
    .by = c(geometry, category),
    n = n()
  ) |> 
  slice_max(n, n = 100)



ggplot() +
  geom_sf(data = nyc_boroughs, fill = "lightgray", color = "black") +
  geom_sf(data = arrests_loc_top_200, aes(color = category), alpha = 1) +
  coord_sf(
    xlim = c(-74.3, -73.7),
    ylim = c(40.5, 40.95),
    expand = FALSE
  ) +
  scale_color_viridis(
    option = "E",
    discrete=TRUE) +
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


