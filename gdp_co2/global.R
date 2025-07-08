if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, countrycode, shiny, shinydashboard, ggiraph, patchwork, hrbrthemes,
               bslib, shinyWidgets, gganimate, plotly, shinycssloaders, dashboardthemes)


# Load dataset
df <- read.csv(file= "data/data.csv", check.names=FALSE)

df <- df %>% rename(
  "Density (P/Km2)" = "Density\\n(P/Km2)",
  "Country" = "Entity",
  "GDP Growth" = "gdp_growth",
  "GDP Per Capita" = "gdp_per_capita",
  "CO2 Emissions (KT) x1000" = "Value_co2_emissions_kt_by_country"
)

# Convert density to numeric for calculations
df$`Density (P/Km2)` <- as.numeric(df$`Density (P/Km2)`)

# Convert emissions for easier interpretation
df$`CO2 Emissions (KT) x1000` <- df$`CO2 Emissions (KT) x1000` / 1000

# Create Population column
df <- df %>%
  mutate(Population = `Density (P/Km2)`*`Land Area(Km2)`)

# Add continent and region columns using countrycode package
df <- df %>%
  mutate(
    continent = countrycode(sourcevar = df$Country, origin = "country.name", destination = "continent"),
    region = countrycode(sourcevar = df$Country, origin = "country.name", destination = "region")
  )

# Calculate CO2 per capita adjusted by population density and land area
df <- df %>%
  mutate(`CO2 per capita` = `CO2 Emissions (KT) x1000` * 1000000 /
           (`Density (P/Km2)` * `Land Area(Km2)`))

# Calculate GDP per CO2 metric for economic efficiency relative to emissions
df <- df %>%
  mutate(`GDP per CO2` = `GDP Per Capita` / (`CO2 per capita` * 1000))

# Calculate year-over-year CO2 emissions growth (%) per country
co2_pct_change_df <- df %>%
  select(Country, Year, `CO2 Emissions (KT) x1000`) %>%
  drop_na() %>%
  group_by(Country) %>%
  mutate(`CO2 Growth` = (`CO2 Emissions (KT) x1000` / lag(`CO2 Emissions (KT) x1000`) - 1) * 100) %>%
  select(Country, Year, `CO2 Growth`)

# Merge CO2 growth back into main dataframe
df <- merge(df, co2_pct_change_df, by = c("Country", "Year"))

# Columns to exclude from some UI selectors
drops <- c("Country","Year","Density (P/Km2)",
           "Land Area(Km2)","Latitude","Longitude",
           "continent","region")
valid_vars <- sort(setdiff(names(df), drops))

