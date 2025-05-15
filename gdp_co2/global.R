library(dplyr)
library(ggplot2)
library(tidyverse)
library(countrycode)
library(shiny)
library(shinydashboard)
library(ggiraph)
library(patchwork)
library(hrbrthemes) # for the `theme_ipsum()`
library(bslib)
library(shinyWidgets)
library(gganimate)
library(plotly)

# import data
df <- read.csv(file= "../data.csv", check.names=FALSE)

df <- df %>% rename("Density (P/Km2)" = "Density\\n(P/Km2)",
                    "Country" = "Entity",
                    "GDP Growth" = "gdp_growth",
                    "GDP Per Capita" = "gdp_per_capita",
                    "CO2 Emissions (KT) x1000" = "Value_co2_emissions_kt_by_country")

# convert density column to numeric
df$"Density (P/Km2)" <- as.numeric(df$"Density (P/Km2)")

df$`CO2 Emissions (KT) x1000` <- df$`CO2 Emissions (KT) x1000`/1000

df <- df %>% 
  mutate("continent" = countrycode(sourcevar = df$Country, 
                                   origin = "country.name",
                                   destination = "continent"))
df <- df %>% 
  mutate("region" = countrycode(sourcevar = df$Country, 
                                origin = "country.name",
                                destination = "region"))

df <- df %>%
  mutate("CO2 per capita" = df$`CO2 Emissions (KT) x1000`*1000000/
             (df$"Density (P/Km2)" * df$"Land Area(Km2)"))

df <- df %>%
  mutate("GDP per CO2" = df$`GDP Per Capita` / (df$`CO2 per capita` * 1000))


co2_pct_change_df <- df %>% select(Country,Year,`CO2 Emissions (KT) x1000`) %>% drop_na() %>%
  group_by(Country) %>% mutate("CO2 Growth" = (`CO2 Emissions (KT) x1000`/lag(`CO2 Emissions (KT) x1000`)-1)*100) %>%
  select(Country, Year, `CO2 Growth`)

df <- merge(df, co2_pct_change_df, by=c("Country", "Year"))


# df <- df %>%
#   rename(
#     "Country" = Entity,
#     "Access to Clean Fuels for Cooking (% of pop)" = Access.to.clean.fuels.for.cooking,
#     "Access to Electricity (% of pop)" = Access.to.electricity....of.population.,
#     "Electricity from Fossil Fuels (TWh)" = Electricity.from.fossil.fuels..TWh.
#     
#   )




drops <- c("Country","Year","Density (P/Km2)",
           "Land Area(Km2)","Latitude","Longitude",
           "continent","region")