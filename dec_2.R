library(tidyverse)
library(stringi)
library(readxl)

df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"

get_numbers <- function(string, colour){
  string %>% 
    str_extract(paste0("([0-9]+)(?>\\s",colour,")")) %>%
    str_remove("[^0-9]+")%>%
    as.numeric()%>% 
    replace_na(0) 
}

#Splitting into game | results 
p1 <- df %>% 
  mutate(
    game_id = row_number(),
    text = str_remove(text, ".*:")
    )%>% 
  group_by(game_id)%>%
  mutate(draws = str_extract_all(text, "[a-z0-9\\s,]*[^;]"))%>% 
  select(-text)%>%
  unnest(draws) %>%
  mutate(green = get_numbers(draws, "green"),
         blue = get_numbers(draws, "blue"),
         red = get_numbers(draws, "red"))

#part 1 solution
get_count <- function(df, nr, ng, nb){
  
  df %>% mutate(
    possible = (max(green) <= ng) * (max(blue)<=nb) * (max(red)<=nr)
    )

}
get_count(p1, 12,13,14)

#part 2 solution, no df change required
get_power_set <- function(df){
  
  df %>% mutate(
    mg = max(green), mb = max(blue), mr = max(red), power = mg*mr*mb
  )%>% ungroup() %>% select(game_id, power)
  %>%unique()%>%select(power)%>%sum()
}
get_power_set(p1)
