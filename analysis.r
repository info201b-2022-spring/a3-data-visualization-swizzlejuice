#loading the data
incarceration<-read.csv(url("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"))
nrow(incarceration)
ncol(incarceration)

#analyzing components of the data - 5 values
#1what was the black to white prison population ratio across all counties per year?
race_ratio = black_prison_pop/white_prison_pop
races_ratio<-round(numbers_one$race_ratio,digits=2)
numbers_one <- mutate(incarceration, race_ratio = races_ratio)
table_ratio<-select(
  numbers_one,
  state,
  year,
  county_name,
  race_ratio,
)
state_ratio_table<-select(
  table_ratio,
  state,
  race_ratio
)

#2how many times did the ratio of black to white prison population exceed 1 over the years across all counties?
sum(numbers_one$race_ratio > 1.00,na.rm="TRUE")

#3in what county was the ratio of black to white total prison population the greatest?
highest_ratio<-max(table_ratio$race_ratio[is.finite(table_ratio$race_ratio)],na.rm="TRUE")
location_highest<-filter(
  table_ratio,
  race_ratio==46
)
#71958

#4what was the total white prison population in the year 2015?
#4what was the total black prison population in the year 2015?
#4what was the total white prison population in the year 1975?
#4what was the total black prison population in the year 1975?
#4what was the total white prison population in the year 2000?
#4what was the total black prison population in the year 2000?
#4what was the total white prison population in the year 1990?
#4what was the total black prison population in the year 1990?
#4what was the total white prison population in the year 2007?
#4what was the total black prison population in the year 2007?
#4what was the total white prison population in the year 1980?
#4what was the total black prison population in the year 1990?
#4what was the total white prison population in the year 1985?
#4what was the total black prison population in the year 1985?
table_count<-select(
  incarceration,
  year,
  state,
  county_name,
  black_prison_pop,
  white_prison_pop
)
year_table<-filter(
  table_count,
  year==2015,
)
white_recent_total<-sum(year_table$white_prison_pop,na.rm="TRUE")
black_recent_total<-sum(year_table$black_prison_pop,na.rm="TRUE")
year_table<-filter(
  table_count,
  year==1975,
)
white_old_total<-sum(year_table$white_prison_pop,na.rm="TRUE")
black_old_total<-sum(year_table$black_prison_pop,na.rm="TRUE")
year_table<-filter(
  table_count,
  year==2000,
)
white_century_total<-sum(year_table$white_prison_pop,na.rm="TRUE")
black_century_total<-sum(year_table$black_prison_pop,na.rm="TRUE")
year_table<-filter(
  table_count,
  year==1990,
)
white_ninety_total<-sum(year_table$white_prison_pop,na.rm="TRUE")
black_ninety_total<-sum(year_table$black_prison_pop,na.rm="TRUE")
year_table<-filter(
  table_count,
  year==2007,
)
white_oseven_total<-sum(year_table$white_prison_pop,na.rm="TRUE")
black_oseven_total<-sum(year_table$black_prison_pop,na.rm="TRUE")
year_table<-filter(
  table_count,
  year==1980,
)
white_eighty_total<-sum(year_table$white_prison_pop,na.rm="TRUE")
black_eighty_total<-sum(year_table$black_prison_pop,na.rm="TRUE")
year_table<-filter(
  table_count,
  year==1985,
)
white_eightyfive_total<-sum(year_table$white_prison_pop,na.rm="TRUE")
black_eightyfive_total<-sum(year_table$black_prison_pop,na.rm="TRUE")

#5in what year and what county had the highest black prison population?
#5in what year and what county had the highest white prison population?
highest_black<-max(table_count$black_prison_pop,na.rm="TRUE")
location_highest_black<-filter(
  table_count,
  black_prison_pop==24818
)

highest_white<-max(table_count$white_prison_pop,na.rm="TRUE")
location_highest_white<-filter(
  table_count,
  white_prison_pop==9945
)
#1998=both

#visualizing the data
#FIRSTCHART
Year <- c(1975, 1985,1990, 2000, 2007, 2015)
Population_Count <- c(5872,143167,181379,377525,492919,428963)
plot(Year, Population_Count, type = "o",main="Black to White Prison Population 1970-2015")
plot(data.frame(Year, Population_Count), type = "o",main="Black to White Prison Population 1970-2015") 
plot(Year ~ Population_Count, type = "o",main="Black to White Prison Population 1970-2015") 
plot(Year, Population_Count, type = "o", cex = 1, pch = 21, bg = "red", col = "blue",main="Black to White Prison Population 1970-2015")
y2 <- c(3158, 128813, 136705, 280734, 437016,429037)
# First line
plot(Year,Population_Count, type = "o",main="Black to White Prison Population 1970-2015")
# Second line
lines(x, y2, type = "o", col = 2,main="Black to White Prison Population 1970-2015")
legend(x = "topleft",          
       legend = c("White Prison Population", "Black Prison Population"),
       title="LEGEND",
       title.col = "blue",
       lty = c(1, 1),           
       col = c(2, 1),           
       lwd = 2) 


#SECONDCHART
year_table<-select(
  numbers_one,
  year,
  state,
  county_name,
  race_ratio
)
chart_two<-select(
  numbers_one,
  year,
  state,
  county_name,
  race_ratio
)
final_table<-filter(
  chart_two,
  year=="2015"
)
recent_av<-.9998
population_table<-select(
  incarceration,
  year,
  state,
  county_name,
  black_pop_15to64,
  black_prison_pop
)
pop_table<-filter(
  population_table,
  year=="2015",
)
pop_sum<-sum(pop_table$black_pop_15to64,na.rm="TRUE")
prison_sum<-sum(pop_table$black_prison_pop,na.rm="TRUE")

x <- c(28229706,428963)
labels <- c("Total Black Population", "Total Black Prison Population")
pie(x, labels, main = "Black Prison Population as a Fraction of Total Black Population in 2015", col = rainbow(length(x)))
legend("bottomleft", c("Total Black Population","Total Black Prison Population"), cex = 0.8,
       title="LEGEND",fill = rainbow(length(x)))

#THIRDCHART
map_table<-select(
  numbers_one,
  state,
  year,
  county_name,
  race_ratio
)
mapping_tab<-filter(
  map_table,
  year=="2015",
  state=="AL"
)

theme_set(theme_bw(base_size=16)) 
us_states <- map_data("state")
us_counties <- map_data("county")
head(us_states)
p + geom_polygon() +
  guides(fill = FALSE)
ratio_state <- mapping_tab %>% 
  group_by(state) 
ga_ratio<-ratio_state%>% drop_na()

state <- map_data("state")

ca_map <- ggplot(data=alabama, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=alabama_county, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  ggtitle('Alabama Map with Counties') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ca_map
