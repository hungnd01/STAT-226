library(babynames)
library(magrittr)
library(Lahman)
library(tidyverse)
library(mdsr)

#Chapter 4 Problem 2-3
babynames %>% select(n > 100)
babynames %>% select(- year)

babynames %>% mutate(length(name) == nchar(name))

glimpse(babynames)
babynames %>% filter(sex == "M") %>% select(-prop)

babynames %>% select(year, sex)
babynames %>% group_by(n) %>% summarize(ave = mean(n))
babynames %>% group_by(n > 100) %>% summarize(total = sum(n))
babynames %>% group_by(n > 100) %>% summarize(total = sum(n))
library(tidyverse)
mtcars %>%
  group_by(cyl) %>%
  summarize(avg_mpg = mean(mpg), am) %>%
  filter(am == 1)


#Chapter 4 Problem 4
#For all years
library(tidyverse)
library(ggplot2)
library(Lahman)
library(lubridate)
team_d <- Teams %>% 
  mutate(BA = H/AB) %>%
  mutate(SLG = (H + 2*X2B + 3*X3B + 4*HR)/AB)

g <- ggplot(data = team_d, aes(x=yearID, y = BA)) 
g + geom_smooth() 

#For after 2015 (noninclusive)
library(tidyverse)
library(ggplot2)
library(Lahman)
library(lubridate)
team_d <- Teams %>% filter(yearID > 2015)%>% 
  mutate(BA = H/AB) %>%
  mutate(SLG = (H + 2*X2B + 3*X3B +4*HR)/AB)

g <- ggplot(data = team_d, aes(x=yearID, y = BA)) 
g + geom_smooth(aes(color = teamID)) 


#Chapter 4 Problem 12A
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(lubridate)

weather <- filter(weather, month == "7")
glimpse(weather)
g <- ggplot(data = weather , aes(x=wind_speed, fill = temp)) 
g + geom_histogram(binwidth = 2)

#Chapter 4 Problem 12B
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(lubridate)


g <- ggplot(weather, aes(x=dewp, y = humid)) 
g + geom_point(size=1)

#Chapter 4 Problem 12c
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(lubridate)


g <- ggplot(data = weather, aes(x=visib, y = precip)) 
g + geom_point(size=1)


#Chapter 4 Problem 14
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(lubridate)

#Find the plane that traveled the most times.
flights_time <- flights %>%
  group_by(tailnum) %>%
  summarize(N=n()) %>%
  arrange(desc(N))

#plane that traveled the most
plane <- toString(flights_time[2, "tailnum"])
print(plane)

#Plot the number of trips per week over the year of plane
#that traveled the most.
flights_g <- flights %>%
  filter(tailnum == plane) %>%
  arrange(time_hour)%>%
  group_by(week = week(time_hour)) %>% 
  summarize(N = n())

g <- ggplot(data=flights_g, aes(x=week,y=N))
g + geom_point(size=2) + geom_smooth()


#Chapter 5 Exercise 2
library(Lahman)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)

#Every player who has accumulated at least 300 home runs 
#and 300 stolen bases
three_hundreds <- Batting %>%
  group_by(playerID) %>%
  summarize(playerID, HR = sum(HR), SB = sum(SB)) %>%
  distinct(playerID, HR, SB) %>%
  filter(HR > 299, SB > 299) %>%
  left_join(People, by = c("playerID" = "playerID")) %>%
  mutate(full_name = paste(nameFirst, nameLast)) %>%
  select(full_name)


#Every pithcer who has accumulated at least 300 wins 
#and 3,000 strikeouts
win_strikouts <- Pitching %>%
  group_by(playerID) %>%
  summarize(playerID, W = sum(W), SO = sum(SO)) %>%
  distinct(playerID, W , SO) %>%
  filter(W > 299, SO > 2999) %>%
  left_join(People, by = c("playerID" = "playerID")) %>%
  mutate(full_name = paste(nameFirst, nameLast)) %>%
  select(full_name)




#Name and year of every of every player who has hit
#at least 50 home runs in one season.
homeruns <- Batting %>%
  filter(HR > 49) %>% 
  left_join(People, by = c("playerID" = "playerID")) %>%
  mutate(full_name = paste(nameFirst, nameLast), HA = H/AB) %>%
  select(playerID, full_name, yearID, HR, HA) %>%
  arrange(HA)

glimpse(homeruns)


#Chapter 5 Exercise 3


#How many planes have a missing date of manufacture?
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(lubridate)
missing_planes <- planes %>%
  filter(is.na(year))

print(nrow(missing_planes))

#What are the five most common manufacturers?
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(lubridate)
common_planes <- planes %>%
  group_by(manufacturer) %>%
  summarize(N = n(), manufacturer) %>%
  distinct(manufacturer, N) %>%
  arrange(desc(N))

#Has the distribution changed over time?
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(lubridate)
distr_planes <- data.frame()
for(n_month in 1:12){
  temp_flights <- flights %>%
    filter(month == n_month) %>%
    inner_join(planes, by = c("tailnum" = "tailnum")) %>%
    count(manufacturer, month) %>%
    mutate(s_count = case_when(n<200 ~ as.integer(sum(which(n<100),2)),
                         TRUE ~ n)) %>%
    mutate(s_manufacturer = case_when(n<200 ~ "Other",
                                      TRUE ~ manufacturer)) %>%
    #distinct(month, s_count, s_manufacturer)

  
    
  distr_planes <- rbind(distr_planes,temp_flights)
}

g <- ggplot(data = distr_planes, aes(x = month, y = s_count))
g + geom_line(aes(color = s_manufacturer))

#Chapter 5 Exercise 4

#What is the oldest plane?
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(lubridate)
oldest_plane <- flights %>%
  inner_join(planes, by = c("tailnum" = "tailnum")) %>%
  distinct(tailnum, year.y,
           type, manufacturer,
           model, engines, seats,
           speed, engine) %>%
  arrange(year.y) %>%
  summarize(tailnum = first(tailnum),
            year = first(year.y),
            type = first(type),
            manufacturer = first(manufacturer),
            model = first(model),
            engines = first(engines),
            seats = first(seats),
            speed = first(speed),
            engine = first(engine))

print(oldest_plane)
  


#How many airplanes that flew from New York City
#are included in the lanes table?
library(nycflights13)
library(tidyverse)
library(ggplot2)
library(lubridate)
included_planes <- flights %>% 
  inner_join(planes, "tailnum") %>%
  distinct(tailnum)

print(nrow(included_planes))
