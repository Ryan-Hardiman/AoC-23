library(tidyverse)
library(stringi)
library(here)
library(readxl)


df<-read_xlsx("C:/Users/RJHar/Documents/Advent of Code/dec_5.xlsx",col_names = FALSE, col_types = "text")
#df<-read_xlsx("C:/Users/RJHar/Documents/Advent of Code/Book3.xlsx",col_names = FALSE, col_types = "text")
names(df) <- "text"
df

n <- rowSums(is.na(df)) == ncol(df)
cs <- cumsum(n) + 1
df <- split(df[!n, ], cs[!n])
df

seeds <- df %>% pluck(1) %>%
  mutate(text = str_extract_all(text, "\\d+"))%>%
  unnest(text) %>% mutate(text = as.numeric(text)) %>%pull(text)


max_value <- df %>% 
  map(~.x %>% pluck(1) %>% str_extract_all("\\d+") %>%
        unlist()%>% as.numeric()%>% max())%>% unlist()%>% max()

min_value <- df %>% 
  map(~.x %>% pluck(1) %>% str_extract_all("\\d+") %>%
        unlist()%>% as.numeric()%>% min())%>% unlist()%>% min()


add_missing_ranges <-function(df, min_value,max_value){
  full_range <- min_value:max_value
  data_range <- df %>% pull(range)
  ranges_to_fill <- full_range[!(full_range %in% data_range)]
  
  tmp_df <- df[0,]
  tmp_df[1, ] <- 0
  tmp_df<- tmp_df %>% mutate(range = list(ranges_to_fill))%>%
    unnest(range)%>%
    mutate(across(names(tmp_df)[!(names(tmp_df)%in% c("range"))]), range)
  
  return(rbind(df, tmp_df))
  
}

unwrap_df <- function(list_df){
  list_df<- list_df[-1]
  names(list_df) <- map(list_df, ~.x %>% head(1) %>% pluck(1))
  list_df <- map(
    list_df,
    ~.x %>% tail(-1) %>%
      mutate(
        quantity = str_extract(text, "\\d+$")%>% as.numeric(),
        destination_start = str_extract(text, "\\d+")%>% as.numeric(),
        destination_end = destination_start + quantity -1,
        source_start = str_remove(text, destination_start%>% as.character()) %>% str_extract( "\\d+")%>% as.numeric(),
        source_end = source_start + quantity -1
        )#%>%
      #select(-text)%>%
      #rowwise()%>%
      #mutate(range = list(source_start:source_end))%>%
      #ungroup()%>%
      #unnest(range)%>%
      #add_missing_ranges(min_value = min_value, max_value = max_value)%>%
      #mutate(map_to = destination_start+range - source_start)
    ) 
  list_df 
}

p1 <- unwrap_df(df)


get_next_id <- function(step, search_id,df){
  
  if(df[[step]] %>% filter(source_start <= search_id & source_end >=search_id) %>% nrow()!=0){
    return(df[[step]] %>%
             filter(source_start <= search_id &
                      source_end >=search_id) %>%
             mutate(new_id = destination_start + search_id - source_start) %>%
             select(new_id)%>%pluck(1)
           )
  }else{return(search_id)}
  
}

get_next_ids <- function(output, step,df){
  force(output)
  tmp_out<-map(output, ~ get_next_id(step,.x,df))
  unlist(tmp_out)->output
  output <- unique(output)
  output <<-output
return(output)
  }

#Part 1
Initialising
output <- seeds
stages <- map(1:length(p1), ~get_next_ids(output,.x,p1))
stages %>% pluck(length(stages))%>%min()




#Part 2 # Initial attempt that would take (days?) to complete
seeds <- df %>% pluck(1) %>%
  mutate(text = str_extract_all(text, "\\d+"))%>%
  unnest(text) %>% mutate(text = as.numeric(text), odd_row = row_number( )%%2 )
seeds

start_seeds <- seeds[seeds %>% pluck("odd_row")==1,1]%>% rename(start = text)
count_seeds <- seeds[seeds %>% pluck("odd_row")==0,1] %>% rename(count = text)
seed_ranges <- cbind(start_seeds, count_seeds)

#attempt to make lists of maximum (minus a few) variables 
seed_ranges %>% rowwise() %>% mutate(vectorable = even / 2^31 - 1 ) #see that we can do it in two

seed_ranges <-seed_ranges %>% rowwise() %>%mutate(start_1 = odd, end_1 = min(2^31 - 1000,even+odd-1),
                       start_2 = ifelse(end_1 == even+odd-1, odd, 2^31 - 999),
                       end_2 = ifelse(end_1 == even+odd-1, odd, even+odd-1))

single_list <- rbind(seed_ranges%>% select(start_1, end_1), seed_ranges%>% select(start_2, end_2))
start_1 <- seed_ranges%>% select(start_1, end_1) %>% rename(start = start_1, end = end_1)
start_2 <- seed_ranges%>% select(start_2, end_2) %>% rename(start = start_2, end = end_2)
ranges <- rbind(start_1, start_2)

seeds <- 1:100000000
output <-seeds
stages <- map(1:length(p1), ~get_next_ids(output,.x,p1))
stages %>% pluck(length(stages))%>%min()
