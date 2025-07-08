library(dplyr)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(shiny)
library(shinydashboard)
library(ggiraph)
library(patchwork)
library(hrbrthemes) # for theme_ipsum()
library(bslib)
library(shinyWidgets)
library(gganimate)
library(plotly)
library(shinycssloaders)
library(dashboardthemes)

# Import data
df <- read.csv(file= "data/data.csv", check.names=FALSE)

df <- df %>% rename(
  "Density (P/Km2)" = "Density\\n(P/Km2)",
  "Country" = "Entity",
  "GDP Growth" = "gdp_growth",
  "GDP Per Capita" = "gdp_per_capita",
  "CO2 Emissions (KT) x1000" = "Value_co2_emissions_kt_by_country"
)

# Convert density column to numeric
df$`Density (P/Km2)` <- as.numeric(df$`Density (P/Km2)`)

df$`CO2 Emissions (KT) x1000` <- df$`CO2 Emissions (KT) x1000` / 1000

df <- df %>%
  mutate(
    continent = countrycode(sourcevar = df$Country, origin = "country.name", destination = "continent"),
    region = countrycode(sourcevar = df$Country, origin = "country.name", destination = "region")
  )

df <- df %>%
  mutate(`CO2 per capita` = `CO2 Emissions (KT) x1000` * 1000000 /
           (`Density (P/Km2)` * `Land Area(Km2)`))

df <- df %>%
  mutate(`GDP per CO2` = `GDP Per Capita` / (`CO2 per capita` * 1000))

co2_pct_change_df <- df %>%
  select(Country, Year, `CO2 Emissions (KT) x1000`) %>%
  drop_na() %>%
  group_by(Country) %>%
  mutate(`CO2 Growth` = (`CO2 Emissions (KT) x1000` / lag(`CO2 Emissions (KT) x1000`) - 1) * 100) %>%
  select(Country, Year, `CO2 Growth`)

df <- merge(df, co2_pct_change_df, by = c("Country", "Year"))

# Columns to exclude from some filters/choices
drops <- c("Country","Year","Density (P/Km2)",
           "Land Area(Km2)","Latitude","Longitude",
           "continent","region")
