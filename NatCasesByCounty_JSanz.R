## Data Visualization QSS17
## Author: Jordan Sanz
##
## Date: 5/15/2020
## Project 2: Cases of Coronavirus in the United States over Time
## WARNING: THE GIF TAKES ABOUT 5-6 HOURS TO RENDER, BECAUSE IT IS SO MUCH DATA AT ONCE (ABOUT 120K ROWS OF DATA). IT TAKES ABOUT 2000 MB OF MEMORY TO RUN.

library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(animation)
library(extrafont)
library(maps)
library(mapproj)
library(gapminder)
library(devtools)
library(gganimate)
library(transformr)
library(grid)

## Makes county map gif

countiesdata <- read_csv("us-counties.csv") # reads in county data
countiesdata

countyfips <- county.fips %>% # separates state and county into two columns
  separate("polyname", c("state", "county"), sep = ",")


countymap <- map_data("county") # reads in map data


countymap <- countymap %>% # renames subregion column to be able to use left join
  rename("county" = subregion)
countymap

countymap <- countymap %>% # joins the map data frame with the fips codes data frame
  left_join(countyfips, by = "county")
glimpse(countymap)

countiesdata # puts unique dates in a data frame to cycle through in for loop
dates <- unique(countiesdata$date)


prettydates <- as.character(format(dates, format= "%B %d, %Y")) # makes pretty dates; practice
prettydates


calicountymap <- countymap %>% # used for figuring out why some counties were not being plotted; there was a 0 in front of the fips codes
  filter(region == "california") %>%
  filter(county == "orange")
calicountymap

countiesdata <- countiesdata %>% # corrects the fips codes to match the map fips codes by taking away the 0
  mutate(fips = if_else(str_detect(fips, "^0"), str_sub(fips, 2), fips))
countiesdata


countymap


# saves plot as an animated gif
 saveGIF({

   ani.options(interval = .20)

   for(i in as.list(dates)){ # for each date in the list of unique dates

     data <- subset(countiesdata, date == i) %>%
       select(fips, cases) %>%
       mutate(fips = as.numeric(fips))# takes just the counties that have cases on that date
     countymapofday <- countymap %>%
       left_join(data, by = "fips") %>%
       mutate(cases = ifelse(!is.na(cases), cases, 0)) %>%
       mutate(cases = as.numeric(cases))

     # gives breaks for the legend labels and colors
     countymapofday$cases <- cut(countymapofday$cases, breaks = c(-Inf, 0, 1, 2, 100, 1000, 5000, 10000, 25000, Inf),
           labels = c("0", "1", "2-50", "51-100", "101-1,000", "1,001-5,000", "5,001-10,000", "10,001-25,000", ">25,001"))

    # makes nice dates in character format
     prettydate <- as.character(format(i, format= "%B %d, %Y"))

    ## makes the plot for that date
     
     p <- ggplot(data = countymapofday, aes(x = long, y = lat, group = group)) +
       geom_polygon(data = countymapofday, aes(fill = cases), color = "gray92") + coord_map() +
      labs(title = "Cases of Coronavirus over Time by County", subtitle = prettydate, fill = "# of Cases", 
           caption = "Source: The New York Times, May 16, 2020.") + theme_classic() +
       theme(text = element_text(family = "Book Antiqua", size = 50, color = "gray20"),
             plot.title = element_text(hjust = .5, vjust = 1.5, size = 100, face = "bold"),
             plot.subtitle = element_text(size = 100, hjust = .5, face="bold"),
             legend.text = element_text(size =  65),
             legend.title = element_text(size = 88, face = "bold"),
            legend.key.size = unit(2.5, "cm"),
             legend.key.height = unit(2.5, "cm"),
             legend.position = c(.92, .15),
             plot.caption = element_text(size = 45, hjust = 0),
             axis.text = element_blank(), axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             axis.line = element_blank()
           ) +
      scale_fill_manual(values = c("#E8E8E8", "#fffcd9", "#fff8ab", "#fffb9c", "#faf35a", "#F1C40F", "#CB4335", "#A93226", "#7B241C"))

     
     print(p) # prints out the plot
     
     if (as.integer(i) == 18397){ # only runs for May 15th, the last data point
       
       for (j in 0:10){
         
         print(p) # makes the gif freeze on the last slide
       }
     }
   print(prettydate) # to keep track of what date the for loop is on currently, for progress's sake
   ani.pause()

   }
   }, movie.name = "Final.gif", ani.width = 3000, ani.height = 3000)