#Chapter 6 Problem 2
library(tidyverse)
select(filter(mtcars, cyl==4), mpg, cyl)

#Chapter 6 Problem 4
library(rvest)
library(tidyverse)
library(janitor)
wiki_url <- "https://en.wikipedia.org/wiki/Economy_of_Vietnam"
tables <- wiki_url %>%
  read_html() %>%
  html_nodes("table")

print(html_table(purrr::pluck(tables, 4)))

table <- tables %>%
  purrr::pluck(5) %>%
  html_table() %>%
  janitor::clean_names() 

g <- ggplot()
g + ggtitle("Nominal GDP of Vietnam (with IMF estimate in 2021-2017)") + 
  geom_line(data = table, aes(x=year, y=gdp_in_bil_us_nominal)) +
  ylab("Nominal GDP (USD)") +
  xlab("Year")

#Chapter 6 Problem 6A
library(rvest)
library(tidyverse)
library(janitor)
library(mosaicData)

table <- HELPfull %>%
  filter(ID <=3) %>%
  select(ID, TIME, DRUGRISK, SEXRISK)


#Chapter 6 Problem 6B
subject_three <- table %>%
  filter(ID == 3) 


#Chapter 6 Problem 6C
library(rvest)
library(tidyverse)
library(janitor)
library(mosaicData)
table <- HELPfull %>%
  select(ID, TIME, DRUGRISK, SEXRISK)
temp_sexrisk <- table %>%
  filter(TIME == 0 | TIME == 6, ID <=3) %>%
  select(ID, TIME, SEXRISK, DRUGRISK) %>%
  pivot_wider(names_from = TIME,
              names_prefix = "SEXRISK_",
              values_from = SEXRISK | DRUGRISK)



#Chapter 6 Problem 6D
library(rvest)
library(tidyverse)
library(janitor)
library(mosaicData)
table_d <- HELPfull %>%
  select(ID, TIME, DRUGRISK, SEXRISK)
temp_sexrisk <- table %>%
  filter(TIME == 0 | TIME == 6) %>%
  select(ID, TIME, SEXRISK, DRUGRISK) %>%
  pivot_wider(names_from = TIME,
              names_prefix = "SEXRISK_",
              values_from = SEXRISK | DRUGRISK)
  
cor(table_d$DRUGRISK_0, table_d$DRUGRISK_6,
    method = "pearson", use = "complete.obs")
cor(table_d$SEXRISK_0, table_d$SEXRISK_6,
    method = "pearson", use = "complete.obs")


#Chapter 6 Problem 7 
ds1 <- data.frame(id = c(1,2,3,1,2,3),
                  group = c("T", "T", "T", "T", "C", "C"),
                  vals = c(4, 6, 8, 5, 6, 10))


Treat <- filter(ds1, group == "T")
Control <- filter(ds1, group == "C")
all <- mutate(Treat, diff = Treat$vals - Control$vals)
all

#More robust approach
ds1 <- data.frame(id = c(1,3,2,1,2,3),
                  group = c("T", "T", "T", "C", "C", "C"),
                  vals = c(4, 6, 8, 5, 6, 10)) %>%
  pivot_wider(names_from = group,
              values_from = vals) %>%
  mutate(diff = T - C) %>%
  summarize(id = id, group = "T",
            vals = T, diff = diff)

ds1
  
#Chapter 6 Problem 8
library(Lahman)
team <- Teams
count_seasons <- function(p_teamID){
  counter <- Teams %>%
    distinct(teamID, yearID) %>%
    filter(teamID == p_teamID) %>%
    nrow()
  return(counter)
}

print(count_seasons("ANA"))

#Chapter 6 Problem 10
library(Lahman)
library(rvest)
library(tidyverse)
library(mosaicData)

team_d <- Teams %>%
  filter(teamID == "CHN") %>%
  select(yearID, HR, HRA) %>%
  pivot_longer(-yearID, names_to = "type", values_to = "home_runs")

g <- ggplot(data = team_d, aes(x = yearID, y = home_runs, color = type))
g + geom_point() +
  geom_line() +
  xlab("Year")+
  ylab("Home Runs") 
  
  
