library(tidyverse)
library(here)
library(readxl)
library(stringi)
library(gtools)
df <-read_xlsx("C:/Users/rh03/Desktop/Personal Files/Advent of Code/dec_15.xlsx",col_names = FALSE, col_types = "text")

names(df) <- "text"
df

reduce_fn <- function(char_list){
  
  reduce(char_list, to_ascii, .init = 0)
  
  
}

to_ascii <- function(value, char){
  
  ((asc(char)[[1]] + value)*17) %%256
  
}

#Part_1

df %>% mutate(captures = str_extract_all(text,"[^, ]+"))%>%
  unnest(captures)%>%mutate(chars = str_extract_all(captures, "."))%>% rowwise() %>% mutate(value = reduce_fn(chars))%>%select(value) %>% sum()
#516657


#Part_2


pack_boxes <- function(data){
  
  data <- data |> as.list()|>unlist()
  
  data<-reduce(data, pull_or_pack_item, .init = list())
 
  if(length(data)==0){return(NULL)}
  
  return(data %>% str_extract("\\d"))
  
  
}



pull_or_pack_item <- function(value, to_box){
  if(is.na(str_extract(to_box, "\\d"))){
    return(value[is.na(str_extract(value, to_box))])
  }
  
  search_for <- str_extract(to_box, "[a-z]+")
  
  if(is.na(str_extract(value, search_for))|>sum()==length(value)){return(value |> append(to_box))}else{
    
    
    find <- str_extract(value,paste0(search_for," \\d"))
    find <- find[!is.na(find)]
    
    value[value == find]<-to_box
    return(value)
    
    
  }
  
}


df %>% mutate(captures = str_extract_all(text,"[^, ]+"))%>%
  unnest(captures)%>%mutate(item = row_number(),chars = str_extract_all(captures, "[a-z]"))%>%
  rowwise() %>%
  mutate(
    box = reduce_fn(chars),
    label = str_extract(captures, "\\d"),
    to_box = paste0(str_extract(captures, "\\w+"),if(!is.na(label)){paste0(" ",label)})
         )|>select( box, to_box)%>%
  group_by(box) %>%
  nest()%>%
  mutate(data = map(data , pack_boxes))%>%
  unnest(data) %>%
  group_by(box) %>%
  mutate(slot = row_number())%>%
  ungroup()%>%
  rowwise()%>%
  mutate(focus_power= prod((box+1) ,slot ,as.numeric(data)))%>%
  select(focus_power)|>sum()
#210906







