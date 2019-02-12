####
#Load packages
####
source("setup.R")

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
    y = "Oil Produced (thousand barrels)"
  )

annual_total_oil


#Plot oil production for top 5 states over time
#Based on their average oil production through time, get the five states that have the highest average production
state_mean_oil_top5 <- raw_data %>%
  #remove na values
  drop_na() %>% 
  group_by(State) %>%
  summarize(avg_yearly_oil_production = mean(oil_produced, na.rm=TRUE), 
            num_years_active = n()) %>%
  arrange(desc(avg_yearly_oil_production)) %>%
  top_n(5, avg_yearly_oil_production)

#create a vector of the top 5 states states, so in the legend they appear in order of avg oil production
top5_vector <- as.vector(state_mean_oil_top5$State)

#plot top 5 states annual production
Top_5_states_annual_production <- raw_data %>%
  right_join(state_mean_oil_top5, by="State") %>%
  select("State", "year", "oil_produced") %>%
  mutate(State=factor(State, levels=top5_vector)) %>%
  ggplot(aes(x=year, y = oil_produced, group=State, color=State)) +
  geom_line(size=1) +
  scale_color_viridis(discrete = TRUE) +
  scale_x_discrete(breaks = c(1981,1986,1991, 1997, 2002,2007, 2012, 2017)) +
  scale_y_continuous(labels= scales::comma) +
  theme_minimal() + 
  labs(
    title = "Top 5 US States in US Oil Production",
    subtitle = "1981 - 2017",
    caption = "Data: US Energy Information Administration; does not include offshore oil production",
    x = "Year",
    y = "Oil Produced (thousand barrels)"
  )

Top_5_states_annual_production

#Make a map of of oil production by state

#Load "fiftystater" which allows you to create a map w/Hawaii and Alaska as an inset
devtools::install_github("wmurphyrd/fiftystater")
library(fiftystater)

#Load the state data frame object 
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