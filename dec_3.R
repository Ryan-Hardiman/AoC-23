library(tidyverse)
library(stringi)
library(readxl)

df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")

names(df) <- "text"


df %>% mutate(nums = str_extract_all(text, "\\d+"))%>% unnest(nums) %>% mutate(check_max = max(nums))

#Numbers are at most 3 digits and so we could:

##1
#Search over each line for a symbol and then return the index of such symbol.
#Then go again finding any number in the [i,j]'s surrounding it - taking care at start and end of string to not get error out of bound
#Then search left and right most twice and trim the resultant,
#Care will need to be taken to not "jump" to a different machine number in this process e.g. if two syms are close to another. 
#One solution would be to do the twice outwards step in two stages  (d) -> (?d?) -> (??d??)

##2
#Search over each line for either a digit+symbol or symboldigit+ and use the following data transformations to get up - down and diagonals:
#Each character becomes an element, we then concatenate after transition to get regex-able strings afterwards
#Transpose = up-down.
#Lag / lead odd / even cols to get diagonal.

#I will go for option 2 as I think I stand a better chance with this.

exploded_df <- df %>% mutate(
  text = str_extract_all(text, pattern ="."),
  row = paste0("r",row_number()) 
  ) %>% unnest(text) %>%
  group_by(row) %>%
  mutate(col = paste0("c",row_number()))%>%
  pivot_wider(names_from = col, values_from = text)%>% 
  ungroup()%>% 
  select(-row)

#NOT USED - Maybe for part 2? 
#t_exploded_df <- exploded_df %>% t() %>% as_tibble()
#names(t_exploded_df) <- t_exploded_df[1,]
#t_exploded_df <- t_exploded_df[-1,]
#

get_from_n <- function(text, length){ #Used for the upper / lower cases (includes diagonals)
  
  first<- substr(text,1,(length+1)/2-1) %>% str_extract("\\d+$")
  middle <- substr(text,(length+1)/2 ,(length+1)/2) %>% str_extract("\\d")
  last<-substr(text,(length+1)/2+1,length)%>% str_extract("^\\d+")
  
  if(is.na(first) & is.na(last) & is.na(middle)){return(0)}
  if(is.na(first) & is.na(last)){return(middle %>% as.numeric())}
  
  first_num <- ifelse(is.na(first),0,first %>% as.numeric())
  last_num <- ifelse(is.na(last), 0, last %>% as.numeric())
  
  if(is.na(middle)){
    return(first_num+last_num)
  }
  mult <-0
  if(is.na(first)){mult <- 1} else if(is.na(last)){mult<--1}
  
  
  return(str_extract(substr(text, 3+(1*mult),5+(1*mult)), "\\d+") %>% as.numeric())

      
}



left_right <- exploded_df %>% unite("text", sep = "")


#Now for up-down & diagonals

long_data <- exploded_df %>% 
  mutate(row = paste0("r", row_number())) %>%
  pivot_longer(-row, names_to = "col", values_to = "text") %>%
  mutate(row= parse_number(row), col = parse_number(col))

#Part1

sym_locations <- long_data %>% mutate(
  sym = str_extract(text, "[^\\.\\d]")
  ) %>% filter(!is.na(sym)) %>% 
  mutate(text_above = substr(left_right[[1]][row-1], start = col-3, stop = col+3),
         text_inline = substr(left_right[[1]][row], start = col-3, stop = col+3),
         text_below = substr(left_right[[1]][row+1], start = col-3, stop = col+3))%>%rowwise%>%
  mutate(above = get_from_n(text_above,7),
         inline = get_from_n(text_inline,7),
         below = get_from_n(text_below,7),
         total = above+inline+below)

sym_locations

#Part2

get_gear_from_n <- function(text, length){ #Used for the upper / lower cases (includes diagonals)
  
  first<- substr(text,1,(length+1)/2-1) %>% str_extract("\\d+$")
  middle <- substr(text,(length+1)/2 ,(length+1)/2) %>% str_extract("\\d")
  last<-substr(text,(length+1)/2+1,length)%>% str_extract("^\\d+")
  
  if(is.na(first) & is.na(last) & is.na(middle)){return(0)}
  if(is.na(first) & is.na(last)){return(middle %>% as.numeric())}
  
  first_num <- ifelse(is.na(first),1,first %>% as.numeric())
  last_num <- ifelse(is.na(last), 1, last %>% as.numeric())
  
  if(is.na(middle)){
    return(first_num*last_num)
  }
  mult <-0
  if(is.na(first)){mult <- 1} else if(is.na(last)){mult<--1}
  
  
  return(str_extract(substr(text, 3+(1*mult),5+(1*mult)), "\\d+") %>% as.numeric())
  
  
}


contains_two <- function(text,length){
  
  first<- substr(text,1,(length+1)/2-1) %>% str_extract("\\d+$")
  middle <- substr(text,(length+1)/2 ,(length+1)/2) %>% str_extract("\\d")
  last<-substr(text,(length+1)/2+1,length)%>% str_extract("^\\d+")
  
  if(!is.na(first) & !is.na(last) & is.na(middle)){return(TRUE)}else{FALSE}
  
}


sym_locations <- long_data %>% mutate(
  sym = str_extract(text, "[*]") #only care about gears
) %>% filter(!is.na(sym)) %>% 
  mutate(text_above = substr(left_right[[1]][row-1], start = col-3, stop = col+3),
         text_inline = substr(left_right[[1]][row], start = col-3, stop = col+3),
         text_below = substr(left_right[[1]][row+1], start = col-3, stop = col+3))%>%rowwise%>%
  mutate(above = get_gear_from_n(text_above,7),
         inline = get_gear_from_n(text_inline,7),
         below = get_gear_from_n(text_below,7),
         total = above+inline+below,
         above_has_2 = contains_two(text_above,7),
         inline_has_2 = contains_two(text_inline,7),
         below_has_2 = contains_two(text_below,7),
         any_2 = ifelse(above !=0 & inline !=0, 
                        TRUE, 
                        ifelse(above !=0 & below !=0,
                               TRUE, 
                               ifelse(inline !=0 & below !=0,
                                      TRUE,
                                      FALSE)
                               )
                        )
         )%>% mutate(above = replace(above, above == 0,1),
                    inline = replace(inline, inline == 0,1),
                    below = replace(below, below == 0,1)) %>%
 mutate(total = prod(above, below, inline, (any_2|above_has_2|inline_has_2|below_has_2))) %>%
   mutate(total = replace(total, total ==1 ,0))

#Part 2
sym_locations %>% select(total) %>% sum()





