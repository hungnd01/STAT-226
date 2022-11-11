#Chapter 7 Problem 2
library(tidverse)
library(flights)

#Chapter 7 Problem 4
library(tidyverse)
library(Lahman)
count_seasons <- function(team) {
  out <- Teams %>%
    filter(teamID == team) %>%
    nrow()
  return(out)
}
bk_teams <- c("BR1", "BR2", "BR3", "BR4", "BRO", "BRP", "BRF")
df4 <- bk_teams %>%
  map_int(count_seasons)
df4 <- tibble(Team = bk_teams, N = df4)


#Chapter 7 Problem 5
library(tidyverse)
library(NHANES)
library(patchwork)
bmi_plot <- function(.data, x_var) {
  ggplot(.data, aes(y = Pulse)) +
    aes_string(x = x_var) + 
    geom_jitter(alpha = 0.3) + 
    geom_smooth() + 
    labs(
      title = paste("Pulse by", x_var),
      subtitle = "NHANES",
      caption = "US National Center for Health Statistics (NCHS)"
    )
}
c("Age", "BMI", "TVHrsDay", "BPSysAve") %>%
  map(bmi_plot, .data = NHANES) %>%
  patchwork::wrap_plots(ncol = 2)


#Chapter 7 Problem 6
library(tidyverse)

record_HR <- Batting %>% 
  filter(yearID > 1899) %>% 
  group_by(yearID) %>% 
  summarise(max_HR = max(HR, na.rm = TRUE)) %>% 
  mutate(top_HR = cummax(max_HR))


df6 <- Batting %>%
  filter(yearID > 1899) %>% 
  group_by(yearID) %>% 
  arrange(desc(HR)) %>%
  select(playerID, yearID, HR) %>%
  group_modify(~ head(.)) %>%
  inner_join(record_HR, by = c("yearID"="yearID")) %>% 
  mutate(beat_record_or_not = HR == top_HR) %>%
  select(playerID, yearID, HR, beat_record_or_not)


g <- ggplot()
g + geom_point(data = df6, aes(x = yearID, y = HR, color = beat_record_or_not))+
  scale_color_manual(values = c("gray", "purple")) +
  theme(legend.position="none",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylab("") +
  xlab("") + 
  ggtitle("Runs batted in 250+ years")




