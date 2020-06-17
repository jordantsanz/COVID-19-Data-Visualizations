## Jordan Sanz
## 5/6/2020
##
## Lab 1
## Coronavirus vs. Chinese Virus

library(tidyverse)
library(stringr)
library(extrafont) # allows for extra fonts
library(maps)
library(mapproj) # allows for map creation
library(scales) # allows for scales in the legend
library(ggpubr) # allows for arranging onto one pdf
library(gridExtra) # allows for putting multiple plots into one pdf

theme_jordan <- function(){
  
  theme(panel.background = element_rect(fill = "gray95"), 
        legend.background = element_rect(fill = "gray95"), 
        text = element_text(family = "Times New Roman", 
                            color = "gray25"), 
        panel.grid = element_blank(), 
        axis.ticks = element_line(color = "gray25"),
        axis.line = element_line(color = "gray80"), 
        plot.title = element_text( size = 29),
        plot.subtitle = element_text(size = 15),
        axis.title = element_text(size = 17), 
        axis.text = element_text(size = 10), 
        axis.title.x = element_text(vjust = -.7),
        )
}

name <- read_csv("virusname.csv")
glimpse(name)

names <- name %>%
  filter(east4 == "Chinese virus")

names <- names %>%
  mutate(polaffil = str_replace_all(polaffil, "A Republican", "Republican")) %>%
  mutate(polaffil = str_replace_all(polaffil, "A Democrat", "Democrat")) %>%
  mutate(polaffil = str_replace_all(polaffil, "An Independent", "Independent")) %>%
  mutate(polaffil = str_replace_all(polaffil, "Don't know", "Other")) %>%
  mutate(polaffil = str_replace_all(polaffil, "Refused", "Other")) %>%
  mutate(polaffil = fct_relevel(polaffil, "Republican", "Independent", "Democrat", "Other")) %>%
  mutate(east5 = str_replace_all(east5, "The major broadcast networks such as ABC, NBC, CBS", "ABC/NBC/CBS")) %>%
  mutate(east5 = str_replace_all(east5, "cnn", "CNN")) %>%
  mutate(east5 = str_replace_all(east5, "msnbc", "MSNBC")) %>%
  mutate(east5 = str_replace_all(east5, "Local television news", "Local News")) %>%
  mutate(east5 = str_replace_all(east5, "Don't know/refused", "Other")) %>%
 mutate(east5 = fct_relevel(east5, "Fox News", "Local News", "ABC/NBC/CBS", "Other", "CNN", "MSNBC"))

plot1<- ggplot(names, aes(x = factor(east5), fill = factor(polaffil))) + geom_bar(aes(y = ..count../sum(..count..))) + 
  scale_fill_manual(values = c("#d30b0d", "#56565B", "#428bca", "#cbcaca"),
                    guide = guide_legend(title.position = "top")) + 
  theme_jordan() + 
  labs(title = 'Racist Terminology: "Chinese Virus" and the News:',
    subtitle = 'Of those that said "Chinese Virus" was the most accurate name for COVID-19, their favorite news station', 
       fill = "Party Affiliation", 
       x = "Favorite News Station", 
       y = "Number of Responses",
       caption = "Data taken from PRRI April 2020 Coronavirus Survey") +
  scale_y_continuous(labels = scales::percent) + theme( legend.position = c(.9, .8),
                                                               legend.text = element_text(size = 14),
                                                               legend.title = element_text(size = 14),
                                                      axis.text = element_text(size = 12),
                                                      axis.title.y = element_text(hjust = .5),
                                                        legend.key.size = unit(.8, "cm"), 
                                                        legend.key.width = unit(.8, "cm")
                                                        
                                                                                             )
plot1


# Map stuff

# getting all of the states to be in the right format with coordinates
 states <- map_data("state")
 glimpse(states)
 
 namesstates <- tolower(state.name)
 namesstates
 states$region <- state.abb[match(states$region, namesstates)]
 states$region
 
 states <- states %>%
   rename("state" = region)
 states

 # match up center and abbreviations
 data.state.center <- as.data.frame(state.center)
 state.center
 data.state.abbr <- as.data.frame(state.abb)
 data.state.abbr <- rename(data.state.abbr, "state" = state.abb)
 data.state.abbr
 data.state.center <- cbind(data.state.center, data.state.abbr)
 data.state.center
 
 # conjoin center with abbreviation
 
name <- name %>%
  left_join(data.state.center, by = "state")
name[, c("x", "y", "state", "east5")]

# conjoin name with states long and lat data for only chinese virus answers
name2 <- name %>%
  group_by(x, y, state) %>%
  mutate(statetotal = n()) %>%
  filter(east4 == "Chinese virus") %>%
  group_by(x, y, statetotal, state) %>%
  summarize(cv = n()) %>%
  mutate(cv = cv / statetotal)
name2[, c("x", "y", "statetotal", "cv")]

# filtered for only coronavirus answers
namecor <- name %>%
  group_by(x, y, state) %>%
  mutate(statetotal = n()) %>%
  filter(east4 == "Coronavirus") %>%
  group_by(x, y, statetotal, state, east4) %>%
  summarize(cv = n()) %>%
  mutate(cv = cv / statetotal)
namecor[, c("x", "y", "statetotal", "cv")]


# states and chinese virus data
states1 <-left_join(states, name2, by = "state")
glimpse(states1)

# states and coronavirus data
states2 <- left_join(states, namecor, by = "state")
glimpse(states2)


# plot of chinese virus popularity within states
mapplot <- ggplot() + 
  geom_polygon(data = states1, aes(group=group, x = long, y = lat, fill = cv), color = "black") +
  coord_map() + scale_fill_gradientn(limits = c(0, .4), colors = c("#F5E0E0", "#F69696", "#A81313", "#750A0A", "#460606"), labels = percent,
                                    guide = guide_colorbar(frame.colour = "black", 
                                                           ticks.colour = "black",
                                                           ticks.linewidth = .4,
                                                           direction = "horizontal",
                                                           title.position = "top"
                                                           
              )) + 
  theme_jordan() + labs(
    title = 'Offensive "Chinese Virus"',
    subtitle = 'Percentage of respondants that said "Chinese Virus" is a more accurate name than "Coronavirus" for COVID-19*',
    fill = "Percent of Respondants",
    caption = "*States filled in grey had no data. Source: PRRI April 2020 Coronavirus survey.") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.key.size = unit(.8, "cm"), 
        legend.key.width = unit(1.2, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(.15, .1),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = .5)
    )
  
mapplot

# plot of coronavirus naming popularity within the us

mapplot2 <- ggplot() + 
  geom_polygon(data = states2, aes(group=group, x = long, y = lat, fill = cv), color = "black") +
  coord_map() + scale_fill_gradientn(limits = c(0, 1), colors = c("#3640ff", "#19547b",  "#e6d695" ), labels = percent,
                                      guide = guide_colorbar(frame.colour = "black", 
                                                             ticks.colour = "black",
                                                             ticks.linewidth = .4,
                                                             direction = "horizontal",
                                                             title.position = "top"
                                                             
                                      )) + 
  theme_jordan() + labs(
    title = 'Coronavirus vs. "Chinese Virus" Talk in the States',
    subtitle = 'Percentage of respondants that said "Coronavirus" is a more accurate name
    than "Chinese Virus" for COVID-19*',
    fill = "Percent of Respondants",
    caption = "*States filled in grey had no data. 
    Source: PRRI April 2020 Coronavirus survey.") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.key.size = unit(.8, "cm"), 
        legend.key.width = unit(1.2, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(.15, .1),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = .5)
  )
mapplot2

## Changing color scale with yellow and red
mapplot4 <- ggplot() + 
  geom_polygon(data = states1, aes(group=group, x = long, y = lat, fill = cv), color = "black") +
  coord_map() + scale_fill_gradientn(limits = c(0, .4), colors = c("#fff3c4", "#f5e298", "#750A0A", "#460606"), labels = percent,
                                     guide = guide_colorbar(frame.colour = "black", 
                                                            ticks.colour = "black",
                                                            ticks.linewidth = .4,
                                                            direction = "horizontal",
                                                            title.position = "top"
                                                            
                                     )) + 
  theme_jordan() + labs(
    title = 'Offensive "Chinese Virus"',
    subtitle = 'Percentage of respondents that said "Chinese Virus" is a more accurate name than "Coronavirus" for COVID-19*',
    fill = "Percent of Respondents",
    caption = "*States filled in grey had no data. Source: PRRI April 2020 Coronavirus survey.") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.key.size = unit(.8, "cm"), 
        legend.key.width = unit(1.2, "cm"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(.15, .1),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        plot.caption = element_text(hjust = .5)
  )

mapplot4

# Code for PDF arrange version of bargraph

bargraph <- ggplot(names, aes(x = factor(east5), fill = factor(polaffil))) + 
  geom_bar(aes(y = ..count../sum(..count..))) + 
  scale_fill_manual(values = c("#d30b0d", "#56565B", "#428bca", "#808080"),
                    guide = guide_legend(title.position = "top")) + 
  theme_jordan()  +
  labs(
       subtitle = 'Favorite News Stations of those Saying "Chinese Virus"', 
       fill = "Party Affiliation", 
       x = "Favorite News Station", 
       y = 'Percentage of Responses',
       caption = "Source: PRRI April 2020 Coronavirus Survey") +
  scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0, .57)) + theme( 
                                                        legend.text = element_text(size = 10),
                                                        axis.text = element_text(size = 7.5), 
                                                        legend.position = c(.8, .8),
                          legend.key.size = unit(.6, "cm"), 
                          legend.key.width = unit(.6, "cm"), 
                          plot.subtitle = element_text(hjust = .5, size = 13),
                          axis.title = element_text(size = 11),
                       
                          plot.caption = element_text(hjust = 1, size = 7),
                          plot.margin = margin(2, 1.4, 2, 1.4, "cm")
                        
                          
                          
                          )

# Code for PDF arrange version of map
mapyellow <- ggplot() + 
  geom_polygon(data = states1, aes(group=group, x = long, y = lat, fill = cv), color = "black") +
  coord_map() + scale_fill_gradientn(limits = c(0, .4), colors = c("#fff3c4", "#f5e298", "#750A0A", "#460606"), labels = percent,
                                     guide = guide_colorbar(frame.colour = "black", 
                                                            ticks.colour = "black",
                                                            ticks.linewidth = .4,
                                                            direction = "horizontal",
                                                            title.position = "top"
                                                            
                                     )) + 
  theme_jordan() + labs(
    subtitle = 'Respondents Using "Chinese Virus" across the USA',
    fill = "Percentage of State Responses",
    caption = 'States filled in grey had no "Chinese Virus" responses. Source: PRRI April 2020 Coronavirus survey.') + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(.167, .13),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8.5),
      
        plot.subtitle = element_text(hjust = .5, size = 14),
        plot.caption = element_text(hjust = .9, vjust = .5, size = 7)
  ) + theme(
                              legend.key.size = unit(.4, "cm"), 
                              legend.key.width = unit(.7, "cm")
                             
                          
                              )
## modifies the title of the pdf grid arrange
title1 = text_grob('Offensive Language: Americans saying "Chinese Virus" over Coronavirus', 
                   family="Times New Roman",
                   size = 20,
                   just = "top",
                   color = "gray25",
                   face = "bold")
grid.arrange(bargraph, mapyellow, nrow = 2,
             top = title1)