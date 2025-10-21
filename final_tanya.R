if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  rvest,
  ggplot2,
  plotly,
  arrow,
  duckdb,
  DBI,
  janitor,
  lubridate,
  scales
)



#------------------create parquet---------------------------


con <- dbConnect(duckdb::duckdb())

#------arrests data to parquet

dbExecute(con, "
    CREATE TABLE arrests AS
    SELECT * FROM read_csv_auto('data/arrest_data.csv');
")
dbExecute(con, "
    COPY arrests TO 'arrest_data.parquet' (FORMAT 'parquet');
")


#------licenses data to parquet
dbExecute(con, "
    CREATE TABLE licenses AS
    SELECT * FROM read_csv_auto(
        'data/licenses.csv',
        delim = ',',
        header = true,
        quote = '\"',
        ignore_errors = true,
        all_varchar = true
    );
")

dbExecute(con, "
    COPY licenses TO 'licenses.parquet' (FORMAT 'parquet');
")



#------shooting data to parquet


dbExecute(con, "
    CREATE TABLE shooting AS
    SELECT * FROM read_csv_auto('data/shooting.csv');
")

dbExecute(con, "
    COPY shooting TO 'shooting.parquet' (FORMAT 'parquet');
")

dbDisconnect(con, shutdown = TRUE) # Disconnect from DuckDB and shut it down

#------------------------------------------------------------
arrests <- arrow::read_parquet("data/arrest_data.parquet") |> 
  clean_names()

licenses <- arrow::read_parquet("data/licenses.parquet") |>
  clean_names()

shooting <- arrow::read_parquet("shooting.parquet") |>
  clean_names()

#----------From alona start--------------------------------------------------

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

#----------From alona end--------------------------------------------------


#------------Monthly trends---------------------------------------
glimpse(arrests)


arrests <- arrests |>
  mutate(arrest_date   = as.Date(arrest_date)) |>  
  mutate(month = month(arrest_date))

monthly_arrests <- arrests |>
  group_by(month) |>
  summarise(total_arrests = n())


ggplot(monthly_arrests, aes(x = month, y = total_arrests)) +
  geom_line(color = "#ffe000", linewidth = 1.2) +
  geom_point(size = 3, color = "darkorange") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous( limits = c(22000, 25000),
                      labels = label_number(scale_cut = cut_short_scale())) +
  labs(
    title = "Number of Arrests in NYC by Month",
    x = "Month",
    y = "Total Arrests"
  ) +
  geom_text(
    aes(label = label_number(scale_cut = cut_short_scale(), accuracy = 0.1)(total_arrests)), # новий спосіб
    vjust = -0.7,
    size = 3.5,
    color = "#e74d11"
  ) +
  theme_minimal(base_size = 14)


#--------------------------------------------------------
glimpse(shooting)

shooting <- shooting |>
  mutate(occur_date   = as.Date(occur_date)) |>  
  mutate(month = month(occur_date))

monthly_shooting <- shooting |>
  group_by(month) |>
  summarise(total_shootings = n())


ggplot(monthly_shooting, aes(x = month, y = total_shootings)) + 
  geom_line(color = "#ffa3a3", linewidth = 1.2) + 
  geom_point(size = 3, color = "#a71b37") + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) + 
  scale_y_continuous(limits = c(40, 110),
                     labels = label_number(scale_cut = cut_short_scale())) + 
  labs( 
    title = "Number of Shootings in NYC by Month", 
    x = "Month", 
    y = "Total Shootings" ) + 
  geom_text(
    aes(label = as.integer(total_shootings)),
    vjust = -0.7,
    size = 3.5,
    color = "#721536",
    
  ) +
  theme_minimal(base_size = 14)



monthly_shooting

#-----------------Gender---------------------------------------

glimpse(arrests)

unique(arrests$perp_sex)

gb_sex_category <- arrests |>
  filter(category != "Other") |>
  group_by(category, perp_sex) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(category) |>
  mutate(percent = 100 * n / sum(n))


gb_sex_category |>
  mutate(category = recode(category,
                                 "Financial/White Collar" = "Financial",
                                 "Public Order/Admin" = "Public/Admin",
                                 .default = category))|>
  mutate(perp_sex = recode(perp_sex, "M" = "Male", "F" = "Female")) |>
  ggplot(aes(
    x = reorder(category, -percent),
    y = percent,
    fill = perp_sex
  )) +
  geom_col(position = "fill") +  
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Male" = "#abc4ff", "Female" = "#fcb0f3")) +
  labs(
    title = "Gender Distribution by Crime Category in NYC",
    x = "Crime Category",
    y = "Proportion of Arrests",
    fill = "Sex"
  ) +
  geom_text(
    aes(
      label = paste0(round(percent, 1), "%"),
      group = perp_sex
    ),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3.5
  ) +
  theme_minimal(base_size = 14)


#------------Race-----------------------------------------
glimpse(arrests)
unique(arrests$perp_race)

my_colors <- c(
  "#f72585",
  "#7209b7",
  "#3a0ca3",
  "#4361ee",
  "#4cc9f0"

)


race_data <- arrests |>
  filter(!is.na(perp_race)) |>
  group_by(perp_race) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(
    percent = n / sum(n) * 100
  )
race_data <- race_data |>
  filter(perp_race!="UNKNOWN" & perp_race!="AMERICAN INDIAN/ALASKAN NATIVE")|>
  arrange(desc(perp_race)) |>
  mutate(
    ypos = cumsum(percent) - percent / 2,
    angle = 90 - 360 * ypos / sum(percent)  
  )

ggplot(race_data, aes(x = 2, y = percent, fill = perp_race)) +
  geom_col() +
  coord_polar(theta = "y") +
  xlim(0.5, 3) +
  geom_text(aes(
    x = 2.6,
    y = ypos,
    label = paste0(round(percent, 1), "%"),
    angle = ifelse(angle < -90, angle + 180, angle), 
    hjust = ifelse(angle < -90, 1, 0)
  ), size = 4) +
  scale_fill_manual(values = my_colors) +
  theme_void() +
  labs(
    title = "Distribution of Arrests by Race in NYC",
    fill = "Race"
  ) +
  theme(
    plot.title = element_text(size = 16)
  )

#------------Age---------------------------------------
glimpse(arrests)
unique(arrests$age_group)


age_data <- arrests |>
  filter(!is.na(age_group)) |>
  group_by(age_group) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(
    percent = n / sum(n) * 100,
    label = paste0(round(percent, 1), "%")
  ) |>
  arrange(desc(age_group)) |>
  mutate(
    ypos = cumsum(percent) - percent / 2,
    angle = 90 - 360 * ypos / sum(percent)
  )

age_colors <- c(
  "<18" = "#9b5de5",
  "18-24" = "#f15bb5",
  "25-44" = "#fee440",
  "45-64" = "#00bbf9",
  "65+" = "#cccccc"
)
age_data <- age_data %>%
  arrange(desc(age_group)) %>%
  mutate(
    ymax = cumsum(percent),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymax + ymin) / 2,
    angle = 90 - 360 * label_pos / sum(percent)  
    
  )


ggplot(age_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2, fill = age_group)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(0, 5)) +  
  geom_text(
    aes(x = 4.78, y = label_pos, label = paste0(round(percent, 1), "%"),
        angle = ifelse(angle < -90, angle + 180, angle)),
    size = 4,
    
  ) +
  scale_fill_manual(values = age_colors) +
  theme_void() +
  labs(
    title = "Distribution of Arrests by Age in NYC",
    fill = "Age Group"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )
