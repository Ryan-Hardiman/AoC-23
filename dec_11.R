library(tidyverse)
library(readxl)
library(stringi)
df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"
df


double_row <- function(row){
  
  ifelse(is.na(str_extract(row$text, "#")), row<-rbind(row,row),row)
  return(row)
  
}

replace_hash_with_num <- function(num, text){
  
  if(!is.na(str_extract(text, "#"))){text<-sub(x = text, pattern = "#", replacement= paste0("h",num))}
  
  return(text)
  
}

map_over_hash <- function(text,start,end){
  
  #reduce(c(start:end), ~replace_hash_with_num(.x,text))
  for (i in start:end){
    text <- replace_hash_with_num(i, text)
  }
  text
}




add_col_space <- function(df){
min_col <- df %>% pull(col) %>% min()
max_col <- df %>% pull(col) %>% max()

expected_seq <- min_col:max_col
expected_seq <-expected_seq[!(expected_seq %in% df$col)]
if(length(expected_seq)==0){return(df)}  
for (i in 1:length(expected_seq)){
  df <- df%>%rowwise()%>% mutate(col = ifelse(col>expected_seq[i]+i-1, col+1, col))
  
}
  return(df)
  
}


do_main_bit <- function(df){
df<-df %>% 
  mutate(n_end = str_count(text, "#"))%>%
  mutate(n_end = accumulate2(n_end, lag(n_end)%>%tail(-1), ~.x +.y)%>%
           unlist()) %>%
  mutate(n_start = lag(n_end) %>%
           replace_na(0)+1) %>%
  rowwise()%>%
  mutate(text = map_over_hash(text, n_start, n_end))%>% 
  mutate(text = str_extract_all(text,"\\.|\\d+"))%>%
  select(text,row)%>%
  ungroup()%>%
  group_by(row)%>%
  unnest(text)%>%
  group_by(row)%>%
  mutate(col =  row_number())%>% 
  ungroup()%>%
  filter(text != ".")%>%add_col_space()
df
}

df<- df %>% split(1:nrow(df))%>% map(~double_row(.x))%>%bind_rows()%>%mutate(row = row_number())%>% do_main_bit()

part_1 <-cross_join(df, df)%>%
  filter(text.x < text.y)%>%
  rename(x = text.x, y = text.y, cx = col.x, cy = col.y, rx = row.x, ry=row.y)%>%
  select(cx,cy,rx,ry,x,y)%>%
rowwise()%>%mutate(dist = abs(rx-ry) + abs(cx-cy))



part_1 %>%pull(dist) %>%sum()
#[1] 9648398


#Part 2
df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"
df


add_col_space <- function(df){
  min_col <- df %>% pull(col) %>% min()
  max_col <- df %>% pull(col) %>% max()
  
  expected_seq <- min_col:max_col
  expected_seq <-expected_seq[!(expected_seq %in% df$col)]
  if(length(expected_seq)==0){return(df)}  
  for (i in 1:length(expected_seq)){
    df <- df%>%rowwise()%>% mutate(col = ifelse(col>expected_seq[i]+(i-1)*999999, col+999999, col))%>%ungroup()
    }
  return(df)
}

add_row_space <- function(df){
  df <- df %>% mutate(row = row_number())
  expected_seq<-  df %>% mutate(check= str_extract(text, "#") %>%
                                  is.na())%>%filter(check ==TRUE)%>%pull(row)
  
  if(length(expected_seq)==0){return(df)}  
  for (i in 1:length(expected_seq)){
    df <- df%>%rowwise()%>% mutate(row = ifelse(row>expected_seq[i]+(i-1)*999999, row+999999, row))%>%ungroup()
  }
  return(df)
}


df<- df%>% add_row_space()%>%do_main_bit()
df

part_2 <-cross_join(df, df)%>%
  filter(text.x < text.y)%>%
  rename(x = text.x, y = text.y, cx = col.x, cy = col.y, rx = row.x, ry=row.y)%>%
  select(cx,cy,rx,ry,x,y)%>%
  rowwise()%>%mutate(dist = abs(rx-ry) + abs(cx-cy))



part_2<- part_2 %>%pull(dist) %>%sum()
format(part_2, scientific = FALSE)
#"618800410814"
