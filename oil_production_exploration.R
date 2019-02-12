####
#Load packages
####
source("setup.R")

#Load us oil production csv 
raw_data <- read.csv("us_oil_production_state.csv", check.names=FALSE, header = TRUE, stringsAsFactors = FALSE) %>%
  #use gather to tidy the data, so one row per year and state
  ##length(.) gets the number of rows in the dataframe
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

###################
#Make a map of of oil production by state in 2016 -------------------------
###################

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

#filter oil data to 2016 and join to spatial data
oil_prod_states_sf_2016 <- raw_data %>%
  filter(year==2016)%>%
  right_join(sf_fifty, by="State") %>%
  select(-id)

#divide data into quantiles
#to get 4 states per section use 8 quantiles
quantiles <- quantile(oil_prod_states_sf_2016$oil_produced, probs=seq(0,1,length.out = 8), na.rm=TRUE)

#create breaks that are slight prettier, easier to read than the raw quantile values
pretty_breaks <- c(250, 2000, 10000, 25000, 50000, 150000)
minVal <- min(oil_prod_states_sf_2016$oil_produced, na.rm=T)
maxVal <- max(oil_prod_states_sf_2016$oil_produced, na.rm=T)

brks<- c(minVal, pretty_breaks, maxVal)
#create a blank label vector and then fill in based using the breaks produced above, rounding the label to the nearest 10th 
labels <- c()

for(idx in 1:length(brks)){
  labels <- c(labels, round(brks[idx+1], 2))
}
#remove NA
labels <- labels[1:length(labels)-1]

#add column to data frame that labels each row in the appropriate quantile/label group
oil_prod_states_sf_2016$breaks_2016 <- cut(oil_prod_states_sf_2016$oil_produced,
                                              breaks=brks,
                                              labels=labels,
                                              include.lowest=TRUE)

#set scale for plotting
brks_scale <- levels(oil_prod_states_sf_2016$breaks_2016)
labels_scale <- rev(format(as.numeric(brks_scale), big.mark = ","))

#plot 2016 oil production
ggplot() +
  geom_sf(data=oil_prod_states_sf_2016, aes(fill=breaks_2016), colour = "darkgray") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(
    x = NULL, 
    y = NULL,
    title = "Crude Oil Production",
    subtitle = "2016",
    caption = "Data: US Energy Information Administration"
  ) +
  scale_fill_manual(
    values=rev(viridis(10, alpha = 0.8)[2:9]),
    breaks = rev(brks_scale),
    name= "Oil Produced (thousand barrels)", 
    drop=FALSE,
    labels=labels_scale, 
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(20/length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T, 
      reverse = T,
      label.position = "bottom"
    )
  ) +
  theme (
    text = element_text(color = "#22211d"), 
    plot.title = element_text(size= 18, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=10, color = "#4e4d47")
  )

