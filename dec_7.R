library(tidyverse)
library(stringi)
library(readxl)

df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"


df <- df %>% mutate(hand = str_extract(text, "[^ ]+"),
             bid = str_extract(text, "[^ ]+$"),
             char_n = str_extract_all(hand, "."),
             jcount = str_count(hand, "J")
             )%>%
  separate(char_n ,map(1:6,~paste0("card_",.x-1))%>%unlist())%>%select(-card_0)


scores <-df%>% mutate(across(starts_with("card"), ~2^(str_count(hand, .x)-1)-1))%>%
  group_by(hand)%>%
  mutate(score_1 = sum(across(starts_with("card")), na.rm = T))%>%
  ungroup()%>% select(hand, score_1)

get_order <- function(value){
  case_when(
   value =="A" ~1,
   value =="K" ~2,
   value =="Q" ~3,
   value =="J" ~4,
   value =="T" ~5,
   !(is.na(str_extract(value, "\\d")))~15 - as.numeric(str_extract(value, "\\d"))
  )
}


order <- df %>% mutate(across(starts_with("card"), ~get_order(.x)))
order             

game <- left_join(order, scores, by = "hand") 

part_1 <- game[with(game, order(-score_1, card_1,card_2,card_3,card_4,card_5)), ]%>% 
  mutate(rank = length(score_1)+1-row_number())

part_1 %>% summarise(result = sum(as.numeric(bid) * rank))


#Part 2
#As with part 1
df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"


df <- df %>% mutate(hand = str_extract(text, "[^ ]+"),
                    bid = str_extract(text, "[^ ]+$"),
                    char_n = str_extract_all(hand, "."),
                    jcount = str_count(hand, "J"))%>%
  separate(char_n ,map(1:6,~paste0("card_",.x-1))%>%unlist())%>%select(-card_0)




get_order <- function(value){
  case_when(
    value =="A" ~1,
    value =="K" ~2,
    value =="Q" ~3,
    value =="J" ~13,
    value =="T" ~4,
    !(is.na(str_extract(value, "\\d")))~14 - as.numeric(str_extract(value, "\\d"))
  )
}

replace_score <- function(jcount, score){
  score_id = case_when(
    score == 0 ~0, #all unique
    score == 2 ~1, #one pair
    score == 4 ~2, #two pair
    score == 9 ~3, #3oc
    score == 11 ~4, #3-2
    score == 28 ~5, #4oc
    score == 75 ~6 #5oc
  )
  
  new_count <- case_when(
    10*jcount + score_id < 10 ~ score,
    10*jcount + score_id == 10 ~ 2 , #if j and no pair make a pair
    10*jcount + score_id == 20 ~ 9 ,
    10*jcount + score_id == 30 ~ 28,
    10*jcount + score_id == 40 ~ 75,
    10*jcount + score_id == 50 ~ 75,
    10*jcount + score_id == 11 ~ 9 , #J=1,2,3 for pair
    10*jcount + score_id == 21 ~ 28,
    10*jcount + score_id == 31 ~ 75,
    10*jcount + score_id == 12 ~ 11,#J=1 for 2 pair
    10*jcount + score_id == 13 ~ 28,#J=1 for 3oc
    10*jcount + score_id == 15 ~ 75,#J=1 for 4oc
    10*jcount + score_id == 21 ~ 28,#J=2,3 for pair
    10*jcount + score_id == 23 ~ 75,
    10*jcount + score_id == 31 ~ 75

  )
  
  new_count
}


scores <-df%>% mutate(
  across(starts_with("card"),
         ~ifelse(.x == "J",0,2^(str_count(hand, .x)-1)-1)
  )
)%>%group_by(hand)%>%
  mutate(score_1 = sum(across(starts_with("card")), na.rm = T))%>%
  ungroup()%>% 
  mutate(score_2 = replace_score(jcount, score_1))%>%
  select(hand,score_1, score_2)


order <- df%>% mutate(across(starts_with("card"), ~get_order(.x)))

game <- left_join(order, scores, by = "hand") 

part_2 <- game[with(game, order(-score_2, card_1,card_2,card_3,card_4,card_5)), ]%>% 
  mutate(rank = length(score_2)+1-row_number())

part_2 %>% summarise(result = sum(as.numeric(bid) * rank))

