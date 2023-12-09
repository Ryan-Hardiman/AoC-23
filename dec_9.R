library(tidyverse)
library(stringi)
library(readxl)

df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"

df<-df %>% 
  mutate(series = str_extract_all(text, "\\d+|-\\d+")%>%map(as.numeric))%>%
  rowwise()%>%
  mutate(max_n = series %>% length(),
         diag = c(0))%>% ungroup()

next_step <- function(step,df){
  df<-df%>%rowwise()%>% mutate(
    diag = list(diag%>%append(series%>%tail(1))),
    left = list(series %>% head(step)),
    right = list(series %>% tail(step)),
    series = list(right-left))
 
 df
}

part_1 <- df
part_2 <- df


itterations <- df$max_n[[1]]-1
for (i in itterations:1){
part_1<- next_step(i,part_1)
}

part_1 <- part_1%>% rowwise() %>% mutate(diag = sum(diag))%>%pull(diag)%>%sum()
part_1
#[1] 2038472161


#Part 2 involves successive subtraction along the left diagonal.
#Reading in Df as avove
part_2

next_step <- function(step,df){
  df<-df%>%rowwise()%>% mutate(
    diag = list(diag%>%append(series%>%head(1))), #one verb change vs alternative of reversing each series
    left = list(series %>% head(step)),
    right = list(series %>% tail(step)),
    series = list(right-left))
  
  df
}

itterations <- df$max_n[[1]]-1
for (i in itterations:1){
  part_2<- next_step(i,part_2)
}

part_2<- part_2 %>% rowwise()%>%mutate(diag = list(rev(diag))) %>% ungroup()#reversing to make reducing easier

cum_diff <- function(value,next_number){
value <- next_number - value
}

part_2 <- map(part_2$diag, ~reduce(.x, cum_diff, .init = 0)*(-1))%>%unlist()%>%sum() # Flipping sign required
part_2
  
  

