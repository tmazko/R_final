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
  plotly,
  osmdata
)

# data prep 
arrests <- arrow::read_parquet("data/arrest_data.parquet") |> 
  clean_names()

shooting <- arrow::read_parquet("data/shooting.parquet") |>
  clean_names()



# this was data exploring 
# arrests |> 
#   distinct(pd_desc) |> 
#   pull()
# arrests |> 
#   glimpse()
# licenses |> 
#   glimpse()
# shooting |> 
#   glimpse()



# categorizing 

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


# data for the first graph 

arrests_loc_top_100 <- arrests |> 
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



# NY map and NY roads 

nyc_boroughs <- counties(state = "NY", cb = TRUE)|> 
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) |> 
  mutate(
    borough_name = case_when(
      NAME == "New York"  ~ "Manhattan",
      NAME == "Kings"     ~ "Brooklyn",
      NAME == "Richmond"  ~ "Staten Island",
      TRUE                ~ NAME 
    )
  )

nyc_bbox <- c(xmin = -74.259, ymin = 40.477, xmax = -73.700, ymax = 40.917)

nyc_streets <- opq(bbox = nyc_bbox) |>
  add_osm_feature(
    key = "highway",
    value = c("motorway", "trunk", "primary", "secondary", "tertiary")
  ) |>
  osmdata_sf()

nyc_streets_lines <- nyc_streets$osm_lines |>
  st_transform(st_crs(nyc_boroughs))

nyc_boroughs_transformed <- st_transform(nyc_boroughs, st_crs(nyc_streets$osm_lines))

nyc_streets_clipped <- st_intersection(nyc_streets$osm_lines, nyc_boroughs_transformed)


# --------------------------------------------------
#end of prepering geodata


# First plot about top 100 arests 

custom_colors <- c(
  "Violent Crime" = "#ff0000",   
  "Property Crime" = "#ffe100",  
  "Public Order/Admin" = "#000000"         
)


ggplot() +
  geom_sf(data = nyc_boroughs_transformed, fill = "#f0f5f9", color = "grey16", linewidth = 0.5) +
  geom_sf(data = nyc_streets_clipped, color = "#b0bac0", linewidth = 0.25) +
  geom_sf_text(
    data = nyc_boroughs_transformed,
    aes(label = borough_name),
    color = "#4a5568",
    fontface = "bold",
    size = 3.5,
    check_overlap = TRUE
  ) +
  geom_sf(
    data = arrests_loc_top_100,
    aes(color = category,
        size = n), 
    alpha = 0.7 
  ) +
  coord_sf(
    xlim = c(-74.25, -73.7), 
    ylim = c(40.5, 40.92),
    expand = FALSE
  ) +
  scale_color_manual(values = custom_colors) +
  scale_size_continuous(
    name = "Number of Arrests", 
    range = c(2, 8)
  ) +
  labs(
    title = "Top 100 Arrest Locations in New York City",
    subtitle = "Points sized by arrest frequency and colored by crime category",
    caption = "Data Source: NYC OpenData | Street map © OpenStreetMap contributors",
    color = "Crime Category" 
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#f0f5f9", color = NA),
    panel.background = element_rect(fill = "#f0f5f9", color = NA),
    legend.position = "right",
    legend.background = element_rect(fill = "transparent"),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2a3439"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#5b6469", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, hjust = 1, color = "gray50"),
    plot.margin = margin(10, 10, 10, 10)
  )





# Data for the second graph of weapon VS Shooting
arrest_weapon <- arrests |> 
  filter(category == "Weapons/Explosives") |> 
  select(new_georeferenced_column) |>
  st_as_sf(wkt = "new_georeferenced_column", crs = 4326) |>
  st_transform(st_crs(nyc_boroughs)) |> 
  summarise(
    .by = new_georeferenced_column,
    n = n()
    ) |> 
  slice_max(n , n =100)
 


shooting_weapon <- shooting |> 
  select(new_georeferenced_column) |>
  st_as_sf(wkt = "new_georeferenced_column", crs = 4326) |>
  st_transform(st_crs(nyc_boroughs)) |> 
  summarise(
    .by = new_georeferenced_column,
    n = n()
  )  |> 
  slice_max(n , n =100)

# --------------------------------

# Second plot 


ggplot() +
  geom_sf(data = nyc_boroughs_transformed, fill = "#f0f5f9", color = "grey16", linewidth = 0.5) +
  geom_sf(data = nyc_streets_clipped, color = "#b0bac0", linewidth = 0.25) +
  geom_sf(
    data = arrest_weapon,
    aes(color = "Weapon Arrests", 
        size = n
        ),
    alpha = 0.7
  ) +
  geom_sf(
    data = shooting_weapon,
    aes(color = "Shooting Incidents", 
        size = n
        ), 
    alpha = 0.7
  ) +
  geom_sf_text(
    data = nyc_boroughs_transformed,
    aes(label = borough_name),
    color = "#4a5568",
    fontface = "bold",
    size = 3.5,
    check_overlap = TRUE
  ) +
  coord_sf(xlim = c(-74.25, -73.7), ylim = c(40.5, 40.92), expand = FALSE) +
  scale_color_manual(
    name = "Incident Type", 
    values = c(
      "Weapon Arrests" = "blue", 
      "Shooting Incidents" = "red" #
    )
  ) +
  scale_size_continuous(
    name = "Number of Incidents", 
    range = c(2, 8)
  ) +
  labs(
    title = "Weapon Arrests and Shooting Incidents in NYC",
    subtitle = "Comparing locations of weapon-related arrests and shooting incidents, sized by frequency.",
    caption = "Data Source: NYC OpenData | Street map © OpenStreetMap contributors"
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#f0f5f9", color = NA),
    panel.background = element_rect(fill = "#f0f5f9", color = NA),
    legend.position = "right",
    legend.background = element_rect(fill = "transparent"),
    pplot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2a3439"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#5b6469", margin = margin(b = 15)),
    plot.caption = element_text(size = 9, hjust = 1, color = "gray50"),
    plot.margin = margin(10, 10, 10, 10)
  )
