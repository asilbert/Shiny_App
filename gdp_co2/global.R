library(dplyr)
library(ggplot2)
library(tidyverse)
library(countrycode)

# import data
df <- read.csv("../data.csv")

# convert density column to numeric
df$Density.n.P.Km2. <- as.numeric(df$Density.n.P.Km2.)

df <- df %>% 
  mutate("continent" = countrycode(sourcevar = df$Entity, 
                                   origin = "country.name",
                                   destination = "continent"))
df <- df %>% 
  mutate("region" = countrycode(sourcevar = df$Entity, 
                                origin = "country.name",
                                destination = "region"))

df <- df %>%
  mutate("co2_per_cap" = df$Value_co2_emissions_kt_by_country*1000/
           (df$Density.n.P.Km2. * df$Land.Area.Km2.))

df <- df %>%
  mutate("gdp_per_co2" = df$gdp_per_capita / df$co2_per_cap)


co2_pct_change_df <- df %>% select(Entity,Year,Value_co2_emissions_kt_by_country) %>% drop_na() %>%
  group_by(Entity) %>% mutate(co2_growth = (Value_co2_emissions_kt_by_country/lag(Value_co2_emissions_kt_by_country)-1)*100) %>%
  select(Entity, Year, co2_growth)

df <- merge(df, co2_pct_change_df, by=c("Entity", "Year"))