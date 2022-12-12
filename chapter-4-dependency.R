## Packages Needed
library(tidyverse)
library(sf)

## Reading in shapefile
ny_shape <- 
  st_read("data/nyctract_acs/NYC_Tract_ACS2008_12.shp") %>%
  st_set_crs(value = 4326) %>%
  janitor::clean_names()


## Poverty Map
brook_shape <- ny_shape %>%
  filter(boroname == "Brooklyn", poptot>200) %>%
  mutate(poverty_rate=(poor/poptot),
         bach_rate = (onlybachel/poptot),
         per_asian = (asian/poptot),
         per_aa = (african/poptot),
         per_his = (hispanic/poptot),
         lab_force_rate =(popinlabou/popover18))

m1 <- brook_shape %>%
  ggplot(aes(fill=poverty_rate)) +
  geom_sf(color="white", lwd=.1) +
  scale_fill_gradient(guide="colorbar", na.value="white") + 
  theme_void() +
  scale_fill_gradient2(midpoint = mean(brook_shape$poverty_rate)) +
  labs(title="Poverty Rate in Brooklyn",
       fill=NULL, caption = "Data Source: 2012 ACS")

m2 <- brook_shape %>%
  ggplot(aes(fill=bach_rate)) +
  geom_sf(color="white", lwd=.1) +
  scale_fill_gradient(guide="colorbar", na.value="white") + 
  theme_void() +
  scale_fill_gradient2(midpoint = mean(brook_shape$bach_rate)) +
  labs(title="Percent Bachelor's Degree in Brooklyn",
       fill=NULL, caption = "Data Source: 2012 ACS")
