library(tidyverse)
library(stringi)
library(readxl)
library(unpivotr)
library(broman)
df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"
input <- separate_wider_delim(df, text, delim = " ", names =c("direction", "duration", "colour"))|>
  mutate(duration = as.numeric(duration))|> split(seq(nrow(df)))

#To solve part 1 we will find the loop, calculate the interior point (using shoelace theorem and picks theorem again)
#and then add this to the length of the loop (-1)



coordinate_math <- function(gather_from, direction =c("lr", "ud"), quantity){
  if(direction == "lr"){pos <- "R";  neg <- "L"}else{ pos <- "D";  neg <- "U"}
  
  ifelse(
    gather_from |> pluck("direction") == pos,
    quantity, 
    ifelse(gather_from|> pluck("direction") == neg,
           -quantity,
           0)
  )
}


add_snake_part <- function(head_to_body, next_body_part){
  
  last_body_part <- head_to_body |> tail(1)#|>pluck(1)
  
  start_coordinates <- list(
    row = last_body_part$row + coordinate_math(last_body_part,"ud",1), 
    col = last_body_part$col + coordinate_math(last_body_part,"lr",1)
  )
  
  end_coordinates <- list(
    row = start_coordinates$row + coordinate_math(next_body_part,"ud",next_body_part$duration-1), 
    col = start_coordinates$col + coordinate_math(next_body_part,"lr",next_body_part$duration-1)
  )
  
  build_body <- tibble(
    row = c(start_coordinates$row , end_coordinates$row),
    col = c(start_coordinates$col , end_coordinates$col),       
    colour = next_body_part$colour,
    direction = next_body_part$direction,
    duration = next_body_part$duration
  )
  # build_body <- split(build_body, seq(nrow(build_body)))
  head_to_body <- head_to_body|> rbind(build_body)#append(build_body)
  head_to_body
}





build_snake <- function(df){

  snake <- tibble(row = 1, col =0, colour = "#FFFFFF", direction = "R", duration= 1 )
  
  out<- reduce(df, add_snake_part , .init = snake)
 
  out
    
}

full_snake <- build_snake(input) |> tail(-1)


#Shoelace Theorem
loop_duration <- full_snake %>% select(colour,duration) %>% unique()%>% select(duration)%>%sum()
dets <-0
for (i in 1:(nrow(full_snake)-1)){ # see that the loop takes no time
  x1 <- full_snake[i,][[1]]
  y1 <- full_snake[i,][[2]]
  x2 <- full_snake[i+1,][[1]]
  y2 <- full_snake[i+1,][[2]]
  dets<- dets %>%append((x1*y2) - (x2*y1)) #determinant - see shoelace and picks theorems
  
  
}

area<-((dets %>%sum())/2) |>ceiling()
area<-abs(area)
#Picks Theorem - finding number of interior points
answer <- area + 1 + loop_duration/2

part_1 <-answer 
part_1
#[1] 67891





#Part_2
#We need only change the input so that the direction and duration come from the hex as follows.
#Additional change to the 

hex_to_direction <- function(hex){
  digit <- substr(hex, nchar(hex)-1, nchar(hex)-1)
  direction<-case_when(
    digit == "0" ~ "R",
    digit == "1" ~ "D",
    digit == "2" ~ "L",
    digit == "3" ~ "U"
  )
}

hex_to_duration <- function(hex){
  hex2dec(substr(hex, 3, nchar(hex)-2)) #Cheating slightly - since doing  str_extract -> case_when -> multiply isn't fun and I know I could do it.
}


input <- map(input, ~.x |> mutate(direction = hex_to_direction(.x$colour), duration = hex_to_duration(.x$colour))) 

full_snake <- build_snake(input) |> tail(-1)
dets <-0


#Shoelace Theorem
loop_duration <- full_snake %>% select(colour,duration) %>% unique()%>% select(duration)%>%sum()
for (i in 1:(nrow(full_snake)-1)){ # see that the loop takes no time
  x1 <- full_snake[i,][[1]]
  y1 <- full_snake[i,][[2]]
  x2 <- full_snake[i+1,][[1]]
  y2 <- full_snake[i+1,][[2]]
  dets<- dets %>%append((x1*y2) - (x2*y1)) #determinant - see shoelace and picks theorems
  
  
}

area<-((dets %>%sum())/2) |>ceiling()
area<-abs(area)

#Picks Theorem - finding number of interior points
answer <- area + 1 + loop_duration/2

part_2 <-answer 
part_2

#94116268857767 low




