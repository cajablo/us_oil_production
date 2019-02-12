####
#Load packages
####
source("setup.R")

####
#Load "fiftystater" which allows you to create a map w/Hawaii and Alaska as an inset
####
devtools::install_github("wmurphyrd/fiftystater")
library(fiftystater)

#Load the object 
data("fifty_states")
as_tibble(fifty_states)

#convert tibble to sf spatial object, with coordinate reference system of 4326 or WGS84
sf_fifty <- st_as_sf(fifty_states, coords = c("long", "lat"), crs = 4326) %>%
  mutate(State = toTitleCase(id)) %>%
  # convert sets of points to polygons
  group_by(id,State, piece) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON") %>%
  # convert polygons to multipolygons for states with discontinuous regions
  group_by(id, State) %>%
  summarize() 


#Load us oil production csv 
raw_data <- read.csv("us_oil_production_state.csv", check.names=FALSE, header = TRUE, stringsAsFactors = FALSE) %>%
  #use gather to tidy the data, so one row per year and state
  gather("year", "oil_produced", 2:length(.))

#Plot how much oil produced yearly in all of US
annual_total_oil <- raw_data %>%
  group_by(year) %>%
  summarize(oil_produced=sum(oil_produced, na.rm = TRUE))%>%
  ungroup() %>%
  ggplot( aes(x = year, y = oil_produced, group=1)) +
  geom_line(color= "deeppink4", size = 1) +
  scale_x_discrete(breaks = c(1981,1986,1991, 1997, 2002,2007, 2012, 2017)) +
  scale_y_continuous(labels= scales::comma) +
  theme_minimal() + 
  labs(
    title = "Total US State Oil Production",
    subtitle = "1981 - 2017",
    caption = "Data: US Energy Information Administration; does not include offshore oil production",
    x = "Year",
    y = "Oil Produced (thousand barrels"
  )

annual_total_oil