#Loading Library
library(tidyverse)
library(skimr)
library(janitor)

#importing Data
Event <- read_csv("athlete_events.csv") 

Region <- read_csv("noc_regions.csv")

#Lets quickly have a sense of our variables and dataframe

# Variables of Event Data frame
colnames(Event)

# Variables of Region Data frame
colnames(Region)

# overview of Event Data frame
glimpse(Event)

# overview of Region Data frame
glimpse(Region)


#Before cleaning our data, let us join the Event & Region
#DataFrame Using the "NOC" Variable and storing in the
#object "Event_Region"

Event_Region <- Event %>% 
  inner_join(Region, by = "NOC")

#DATA CLEANING

#Variable names
colnames(Event_Region)

#cleaning the variable names
Event_Region <- clean_names(Event_Region)


#structure of the data frame
str(Event_Region)

# Lets us convert the variables sex, season and medal
#to factor from character since they are categorical
#variables

#sex character variable to factor variable
Event_Region$sex <- as.factor(Event_Region$sex)

#season character variable to factor variable
Event_Region$season <- as.factor(Event_Region$season)

#medal character variable to factor variable
Event_Region$medal <- as.factor(Event_Region$medal)

#let's verify that our variables have been converted
#successfully
sapply(Event_Region, class)


# We are going to be answering the following questions

#1. How many Olympic games have taken place?

n_distinct(Event_Region$games)


#2. List all the Olympic games along with the cities where they were held.

Event_Region %>% 
  select(games, city) %>%
  distinct() %>% 
  arrange(games)

#The first Olympic game ever was in 1896 Summer which was hosted in Athina city



#3. List every region that took part in the 1896 Summer Games.

Event_Region %>% 
  select(games, region) %>% 
  distinct() %>% 
  filter(games=="1896 Summer")

# 12 Region participated in 1896 Summer game among which were Greece, Uk, Switzerland etc


#4. Mention the total number of countries that competed in each Olympic game.

Event_Region %>% 
  select(games, total_countries = region) %>% 
  distinct() %>% 
  aggregate(total_countries~games, FUN = length)


#5. Which Olympic game has the most and least countries participating?

Event_Region %>% 
  select(games, region) %>% 
  distinct() %>% 
  aggregate(region ~ games, FUN = length) %>% 
  summarise(lowest_country_partcipation  = c(min(games),min(region)),
            highest_country_partcipation= c(max(games),max(region)))


#6.Which nation or country has competed in each and every Olympic Game?

Event_Region %>% 
  select(region, games) %>%
  distinct() %>% 
  aggregate(games~region, FUN = length) %>% 
  filter(games == 51)
#-------------------------------------------------------------------------

#7. Get the top 5 medal-winning nations from the Olympics. Success is measured 
#by the total number of medals earned.

Event_Region %>% 
  select(country = region, total_medals = medal) %>% 
  aggregate(total_medals ~ country, FUN = length) %>% 
  arrange(desc(total_medals)) %>% 
  slice(1:5)



#8. How many gold, silver, and bronze medals did each nation win in total? 
#A nation must take home at least one of each medal.


Event_Region %>% 
  group_by(Country = region, medal) %>% 
  filter(medal %in% c("Gold", "Silver", "Bronze")) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = "medal", values_from = "n") %>% 
  select(Gold, Silver, Bronze) %>% 
  na.omit() %>% 
  arrange(desc(Gold))


#9.List all of Nigeria's gold, silver, and bronze medals from each Olympic game.

Event_Region %>% 
  group_by(games, Country = region, medal) %>% 
  filter(medal %in% c("Gold", "Silver", "Bronze")) %>% 
  summarise(n=n()) %>% 
  filter(Country == "Nigeria") %>% 
  pivot_wider(names_from = "medal", values_from = "n")%>% 
  select(Gold, Silver, Bronze) %>% 
  mutate(Gold = replace_na(Gold, 0), 
         Silver= replace_na(Silver, 0),
         Bronze = replace_na(Bronze, 0)) %>% 
  arrange(desc(Gold))


#10.List the total number of gold, silver, and bronze medals each nation has earned at each Olympic game.

Event_Region %>%
  group_by(games, region, medal) %>% 
  summarise(n=n()) %>% 
  na.omit() %>% 
  pivot_wider(names_from = "medal", values_from = "n") %>% 
  select(Gold, Silver, Bronze) %>%
  mutate(Gold = replace_na(Gold, 0), 
         Silver= replace_na(Silver, 0),
         Bronze = replace_na(Bronze, 0))

#11. Which country has never won a gold medal but has instead received a silver or bronze?

Event_Region %>% 
  group_by(region, medal) %>% 
  summarise(n=n()) %>% 
  na.omit() %>% 
  pivot_wider(names_from = "medal", values_from = "n")%>%
  select(Gold, Silver, Bronze) %>%
  mutate(Gold = replace_na(Gold, 0), 
         Silver= replace_na(Silver, 0),
         Bronze = replace_na(Bronze, 0)) %>% 
  filter(Gold == 0 & Silver > 0 & Bronze > 0)


#12. Which sport has Nigerian athletes won the most medals?

Event_Region %>% 
  select(region, sport, medal) %>% 
  aggregate(medal ~ region + sport , FUN = length) %>% 
  filter(region == "Nigeria") %>% 
  arrange(desc(medal))

#13. Analyze each Olympic games in which Nigeria won a football medal, noting the total number of medals obtained.

Event_Region %>% 
  select(team, sport, games, medal) %>% 
  aggregate(medal ~ team + sport + games , FUN = length) %>% 
  filter(team == "Nigeria" & sport == "Football") %>% 
  arrange(desc(medal))


# SPORT ANALYSIS

#14. Name the sport that was played at every summer Olympic Games.

Event_Region %>% 
  select(sport, season) %>% 
  distinct() %>% 
  filter(season == "Summer")


#15.Which Olympic game have only ever been played once?

Event_Region %>% 
  select(sport, no_of_games = games) %>% 
  distinct() %>% 
  aggregate(no_of_games~sport, FUN = length) %>% 
  filter(no_of_games == 1) %>% 
  View()


#16. How many different sports are featured in each Olympic game?

Event_Region %>% 
  select(games, sport) %>% 
  distinct() %>% 
  aggregate(sport ~ games, FUN = length) %>% 
  arrange(desc(sport))

#GENDER AND ATHLETHE ANALYSIS

#17. Which gender took part in the Olympic games the most?

Event_Region %>% 
  group_by(sex) %>% 
  summarise(total=n())

#18. What is the Total Number of Participants Over the Year?

Event_Region %>% 
  select(year, season, total_participant = sex) %>% 
  aggregate(total_participant ~ year + season, FUN = length) %>% 
  arrange(year)

#19. What was the gender participation breakdown throughout the course of the year?

Event_Region %>%
  group_by(year, season, sex) %>% 
  summarise(n=n()) %>% 
  pivot_wider(names_from = "sex", values_from = "n")%>%
  select(male = M, female =F) %>% 
  mutate(male = replace_na(male, 0),
         female = replace_na(female,0))


#20. What were the Medals totaling gold, silver, and bronze received by each gender?

Event_Region %>% 
  group_by(sex, medal) %>% 
  summarise(n=n()) %>% 
  drop_na() %>%
  pivot_wider(names_from = "medal", values_from = "n") %>% 
  select(Gold, Silver, Bronze) %>% 
  mutate(total = (Gold + Silver + Bronze))



#21.List the gold medalists that are under 15 years old.

Event_Region %>% 
  select(name, age, region, medal) %>% 
  filter(age < 15 & medal == "Gold") %>%
  arrange(region, age) %>% 
  distinct()

#22. Who are the oldest athletes to have won a gold medal?

Event_Region %>% 
  select(name, sex, age, team, games, 
         city, sport, event, medal, region) %>% 
  arrange(desc(age)) %>% 
  filter(medal == "Gold", age == 64)


#23. Who are the top 10 athletes who have won the most
#gold medals

Event_Region %>% 
  select(name, total_gold_medals = medal, team) %>% 
  filter(total_gold_medals == "Gold") %>% 
  aggregate(total_gold_medals ~ name + team, FUN = length) %>% 
  arrange(desc(total_gold_medals)) %>% 
  slice(1:10)

#24. Get the top 10 athletes by number of medals earned (gold, silver, or bronze).

Event_Region %>% 
  select(name, total_medals = medal, team) %>% 
  aggregate(total_medals ~ name + team, FUN = length) %>% 
  arrange(desc(total_medals)) %>% 
  slice(1:10)

#25. What age group participated in the game the most?

Event_Region %>% 
  group_by(age_group = age) %>% 
  summarise(count=n()) %>%
  na.omit() %>%
  arrange(desc(count))

#the most age group is 23 years of age with 21,848 participant

#----------------------THE END---------------------------------





  






  
  







  

 
 
  
  
  
  
  
 
  
  
  
  
 
  























  



  





