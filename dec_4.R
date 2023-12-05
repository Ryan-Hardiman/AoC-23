library(tidyverse)
library(stringi)
library(here)
library(readxl)


df<-read_xlsx("C:/Users/RJHar/Documents/Advent of Code/dec_4.xlsx",col_names = FALSE, col_types = "text")
#df<-read_xlsx("C:/Users/RJHar/Documents/Advent of Code/Book3.xlsx",col_names = FALSE, col_types = "text")
names(df) <- "text"
df


result <- df %>% mutate(card_id = str_extract(text, ".*:"),
              winning_nums = str_remove(text, card_id) |>
                str_extract("^.*?(?=[\\|])")|> 
                str_extract_all("\\d+"),
              check_nums = str_remove(text, ".*\\|") |> str_extract_all("\\d+"))|>
  group_by(card_id)|>
  unnest(winning_nums)|> 
  unnest(check_nums) |>
  mutate(match = winning_nums == check_nums) |> 
  mutate(winnings = floor(2^(sum(match)-1)))|>
  ungroup(card_id)|>
  select(card_id,winnings)|>
  unique()|>
  select(winnings)|>sum()


#part 1
result #22674

#Onto part 2
#recurse <- function(lst){
#  print(paste0("current list is" ,unlist(lst)))
#  if(lst |> pluck(1) ==0){lst}else{map(lst, ~result$cards[[.x]] |> recurse())}
#}

result <- df %>% mutate(card_id = str_extract(text, ".*:"),
                        winning_nums = str_remove(text, card_id) |>
                          str_extract("^.*?(?=[\\|])")|> 
                          str_extract_all("\\d+"),
                        check_nums = str_remove(text, ".*\\|") |> str_extract_all("\\d+"))|>
  group_by(card_id)|>
  unnest(winning_nums)|> 
  unnest(check_nums) |>
  mutate(match = winning_nums == check_nums,
         card_id = parse_number(card_id))|>
  mutate(winnings = sum(match))|>
  ungroup(card_id)|>
  select(card_id, winnings)|>
  unique()|>
  rowwise()|>
  mutate(cards = 
           ifelse(winnings == 0,
                  list(0),
                  list(seq((card_id + 1), (card_id + winnings), by = 1))))|>
  ungroup()


#result<- result|>mutate(recursion = list(recurse(recursion))) |> mutate(check = length(recursion)*card_id)




#making a df

out_df <- tibble(card_id = result$card_id, count = 1)%>% as.matrix()



get_cards<- function(row_number){
  result %>% filter(card_id == row_number) %>% pull(cards) %>% pluck(1)
}

add_count <- function(step, out_df){
  force(out_df)
 ids <- get_cards(step)
 map(ids, ~get_count(out_df, step , .x)) %>%pluck(1)
 
    
}

get_count <- function(out_df, step, id){
  if(id == 0){return(out_df)}
  out_df[id,2]<<- out_df[id,2] + out_df[step,2]
}


map(result$card_id, ~add_count(.x,out_df)) %>% pluck(result$card_id %>% length())->final



