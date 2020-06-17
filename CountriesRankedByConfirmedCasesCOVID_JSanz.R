## Data Visualization (QSS17) Spring 2020
##
##
## Name: Jordan Sanz
## Date: May 15 - May 22, 2020
## Project: Ranking Countries by Confirmed COVID-19 Cases

library(ggplot2)
library(tidyverse)
library(maps)
library(mapproj)
library(dplyr)
library(stringr)
library(ggmap)
library(extrafont)
library(gganimate)
library(magick)
library(gifski)

data <- read_csv("data.csv")
data


##### Making the Bar Graph Data #####

## Groupings for Chinese provinces:
## North China: Beijing, Tianjin, Hebei, Shanxi, Inner Mongolia
## Northeast China: Liaoning, Jilin, Heilongjiang
## East China: Shanghai, Jiangsu, Zhejiang, Anhui, Fujian, Jiangxi, Shandong
## South Central China: Henan, Hubei, Hunan, Guangdong, Guangxi, Hainan, Hong Kong, Macau,
## Southwest China: Chongqing, Sichuan, Guizhou, Yunnan, Tibet
## Northwest China: Shaanxi, Gansu, Qinghai, Ningxia, Xinjiang


world_confirmed <- read_csv("confirmed.csv")
glimpse(world_confirmed)

## groups China by location of province
gathered_confirmed <- world_confirmed %>%
  mutate(`Country/Region` = case_when(
    `Province/State` %in% c("Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia") ~ "North China",
    `Province/State` %in% c("Liaoning", "Jilin", "Heilongjiang") ~ "Northeast China",
    `Province/State` %in% c("Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong") ~ "East China",
    `Province/State` %in% c("Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", "Hong Kong", "Macau") ~ "South Central China",
    `Province/State` %in% c("Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet") ~ "Southwest China",
    `Province/State` %in% c("Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang") ~ "Northwest China",
    TRUE ~ `Country/Region`
  )) %>%
  gather(-`Province/State`, -`Country/Region`, key = "Day", value = "Cases") %>%
  mutate(Country = `Country/Region`) %>%
    group_by(Day, Country) %>%
    summarize(Cases = sum(Cases)) %>%
    ungroup() %>%
  mutate(Day = as.Date(Day, format = "%m/%d/%y")) %>% # puts dates in correct date format
  mutate(Cases = as.integer(Cases))
gathered_confirmed


## ranks each country based on date and number of cases
ranked_confirmed <- gathered_confirmed %>%
  group_by(Day) %>%
  mutate(rank = rank(-Cases, ties.method = "last")) %>% # ranks the countries by amount of cases, breaks ties by thing that appeared first
  group_by(Country) %>%
  filter(rank <= 15) %>% # ranks only the top 12
  ungroup() %>%
  mutate(Country = case_when( 
    Country == "US" ~ "USA", # changes US label to USA
    Country == "Taiwan*" ~ "Taiwan", # gets rid of asterisk next to Taiwan
    TRUE ~ Country
  ))

## colors used in animation of the plot (45 colors)
colors <- c("#1F159D", "#A6358E", "#C50808", "#0E0202", "#15C818", "#929D92", "#D94037", 
            "#37D9D7", "#D4CF20", "#19C2E8", "#1961E8", "#7E8F11", "#DF9D1C", "#B529B1", 
            "#008c45", "#BFB5B3", "#797CC5", "#C55DCD", "#7447DE", "#B7453A", "#D4CF20",
            "#D4CF20", "#D4CF20", "#D94916", "#23AE0F", "#EB2A18", "#650FAE", "#276F4A",
            "#F2913B", "#D4CF20", "#D4CF20", "#BF9B30", "#287CBF", "#4BBDCE", "#DD1B10", 
            "#1E257D", "#D3452E", "#1B7B59", "#4619C5", "#3c3b6e", "#E1E32D", "#E6A531", 
            "#212E54", "#21542A", "#48A067")


## Making the static plot for animation
theplot <- 
  ggplot(ranked_confirmed, 
    aes(rank, group = Country, 
        fill = as.factor(Country),
        color = as.factor(Country))
        ) + 
  geom_col(aes(y = Cases/2)) + # changes scale of flipped y axis to allow the panels to move
  transition_states(Day, wrap = FALSE) +
  view_follow(fixed_x = TRUE) + # moves view with bars
  coord_flip(clip = "off") + # flips the axes
  labs(title = "Countries Ranked by Confirmed Cases of COVID-19", 
       subtitle = "Date: {closest_state}", 
       caption = "Source: COVID-19 Data Repository by the CSSE at Johns Hopkins University") + 
  theme_minimal() +
  theme(axis.title = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks = element_blank(), 
        text = element_text(family = "Calibri"),
        plot.title = element_text(size = 32, hjust = .5, color = "black"),
        plot.subtitle = element_text(size = 32, hjust = .5, color = "black"),
        plot.caption = element_text(hjust = 1, color = "black"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "none",
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.margin = margin(2, 5, 2, 6, "cm")) + 
  scale_x_reverse() + # flips scale of x so that highest amt of cases is on top instead of bottom
  geom_text(aes(y = 0, label = c(Country)), 
  hjust = 1.1, family = "Calibri", size = 7.2, fontface = "bold") + 
  geom_text(aes(y = Cases/2, label = format(round(Cases))), # rounds the label display to the nearest integer so that it doesn't display a decimal
            hjust = -.7, family = "Calibri", fontface = "bold", size = 6) + 
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = colors)



##### Animation Call #####
animate(theplot, 470, fps = 10, width = 1200, height = 800, detail = 100, renderer = gifski_renderer("RankedConfirmedCasesByCountryFinal.gif"), end_pause = 25, start_pause = 10)