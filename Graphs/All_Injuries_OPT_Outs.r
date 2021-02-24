
###Load packages
```{r}
library(tidyverse)
library(devtools)
library(dplyr)
library(ggimage)
library(lubridate)
library(nflfastR)
library(ggplot2)
library(teamcolors)
library(RCurl)
```

###Load data from GitHub
```{r}
PFR_2020_Injuries <- read.csv("https://raw.githubusercontent.com/jchernak96/NFL-Injury-Data-PFR-/master/Data/PFR_2020_Injuries.csv")
```

###Filter dataset to only include regular season and games missed. Also, filter out special teams and rearrange data to be wide for graphing purposes
```{r}
Injury_report <- PFR_2020_Injuries %>% 
  filter(Active_Inactive == "Out", Week <18) %>%
  group_by(Team, Side.of.Ball)%>%
  summarize(Games_Lost = n())  %>%
  ungroup() %>%
  arrange(desc(Games_Lost)) %>%
  filter(Side.of.Ball != "Special Teams") %>%
  pivot_wider(names_from = Side.of.Ball, values_from = Games_Lost)

###Rename colors dataset to merge with injury data
teams_colors_logos <- teams_colors_logos %>% 
  rename(Team = "team_name") 

###Merge data
Injuries_Final <- merge(Injury_report, teams_colors_logos, by="Team")

```

###Graph our data
```{r}
Injuries_Final %>%
  ggplot(aes(x=Offense, y=Defense))+
  geom_image(aes(image = team_logo_wikipedia)) +
  theme_light()+
  theme(plot.title = element_text(color="black", size=8, face="bold"))+
  coord_cartesian(clip = "off") +
  theme(plot.title = element_text(size = 10, face = "bold"),
  plot.subtitle = element_text(size = 8))+
  theme(plot.background = element_rect(fill = "gray97"))+
  theme(panel.background = element_rect(fill = "gray97"))+
  labs(title = "Man Games Lost 2020 Regular Season - Includes COVID Opt Outs",
       subtitle = "Total Count of Games Missed due to Injury or COVID",
       caption = "Plot: PatriotsStatsR, Data: PFR")+
    ylab("Defensive Games Lost")+
  xlab("Offensive Games Lost")+
  geom_hline(yintercept = mean(Injuries_Final$Defense), linetype = "dashed")+
  geom_vline(xintercept = mean(Injuries_Final$Offense), linetype = "dashed")+
  annotate("Text", x = 150, y = 25, label = "High Offensive Injuries, Low Defensive", size = 3)+
  annotate("Text", x = 150, y = 200, label = "High Offensive Injuries, High Defensive", size = 3)+
  annotate("Text", x = 50, y = 200, label = "High Defensive Injuries, Low Offensive", size = 3)+
  annotate("Text", x = 50, y = 25, label = "Low Offensive Injuries, Low Defensive", size = 3)+
  ggsave("games_lsot.png")

```








