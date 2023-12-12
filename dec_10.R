library(tidyverse)
library(stringi)
library(readxl)

df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"

df<- df %>% mutate(text = str_replace_all(text, "7" ,"?")) 

exploded_df <- df %>% mutate(text = str_extract_all(text, "."),
                             row = row_number())%>%
  unnest(text)%>%
  group_by(row)%>%
  mutate(col = paste0("c", row_number()))%>%
  pivot_wider(names_from = col, values_from = text)%>%
  ungroup()%>%select(-row)%>% as.matrix()


index <- df %>% mutate(text = str_extract_all(text, "."),
                       row = row_number())%>%
  unnest(text)%>%
  group_by(row)%>%
  mutate(col = row_number())%>%rowwise()%>%
  mutate(text = list(c(row,col)))%>%ungroup()%>%pivot_wider(names_from = col, values_from = text)%>%
  ungroup()%>%select(-row)%>% as.matrix()

get_long_data <-function(expl_df){ 
  max_c <- ncol(expl_df)
  max_r <- nrow(expl_df)
  
  
  df<- expl_df %>% as_tibble()%>%
    mutate(row = paste0("r", row_number()))%>%
    pivot_longer(-row, names_to = "col", values_to = "text")%>%
    rowwise()%>%  mutate(row = parse_number(row),
                         col = parse_number(col))%>% ungroup()
  df
}

long_data <- get_long_data(exploded_df)




decision <- function(curr_pos, prev_pos, steps){
  xchange <- curr_pos[1] - prev_pos[1] 
  ychange <- curr_pos[2] - prev_pos[2]
  
  
  up <- ifelse(curr_pos[1] ==1, ".",exploded_df[curr_pos[1]-1,curr_pos[2]])
  up <- ifelse(up %in% c("?", "F", "|","S") & exploded_df[curr_pos[1], curr_pos[2]]%in% c("J","|", "L"), up, NA)
  down <- ifelse(curr_pos[1] ==nrow(exploded_df), ".",exploded_df[curr_pos[1]+1,curr_pos[2]])
  down <- ifelse(down %in% c("L", "J", "|","S") & exploded_df[curr_pos[1], curr_pos[2]]%in% c("?","|","F"),down, NA)
  left <- ifelse(curr_pos[2] ==1, ".",exploded_df[curr_pos[1],curr_pos[2]-1])
  left <- ifelse(left %in% c("F", "L","-","S") & exploded_df[curr_pos[1], curr_pos[2]]%in%c("-","J","?"),left, NA)
  right <- ifelse(curr_pos[2] ==ncol(exploded_df), ".",exploded_df[curr_pos[1],curr_pos[2]+1])
  right <- ifelse(right %in% c("?", "J","-","S") & exploded_df[curr_pos[1], curr_pos[2]]%in%c("F","L","-"),right, NA)
  
  
  
  
 current_direction<- case_when(
    xchange == -1 ~"up",
    xchange ==  1 ~ "down",
    ychange == -1 ~ "left",
    ychange ==  1 ~ "right"
  )
  
 look<-case_when(
   current_direction == "left"  ~ c("left", "up","down")[!is.na(c(left, up,down))]%>%pluck(1),
   current_direction == "right" ~ c("right", "up","down")[!is.na(c(right, up,down))]%>%pluck(1),
   current_direction == "up"    ~ c("up", "left","right")[!is.na(c(up, left,right))]%>%pluck(1),
   current_direction == "down"  ~ c("down", "left","right")[!is.na(c(down, left,right))]%>%pluck(1)
   )
 
 
 exploded_df[curr_pos[1], curr_pos[2]]
 
go <- case_when(
  look == "left" ~ c(curr_pos[1], curr_pos[2]-1),
  look == "right" ~ c(curr_pos[1], curr_pos[2]+1),
  look == "up" ~ c(curr_pos[1]-1, curr_pos[2]),
  look == "down" ~ c(curr_pos[1]+1, curr_pos[2]),
)

steps <- append(steps, list(go%>%as.integer()))


 if(exploded_df[go[1], go[2]]=="S"){ return(steps)}


prev_pos <-curr_pos
curr_pos <- go
rm(go)
rm(left)
rm(right)
rm(up)
rm(down)
rm(look)
rm(current_direction)
rm(xchange)
rm(ychange)

return(list(curr_pos,prev_pos,steps))

}






s_location <- index[exploded_df == "S"]%>% pluck(1)

choice <- c("up", "down", "left", "right")

steps <- list(s_location)





looper<-decision(c(s_location[1], s_location[2]+1), s_location, steps)

while (length(looper)==3){
  
  looper <- decision(looper[[1]],looper[[2]],looper[[3]])
  
}

part_1 <- length(looper)/2
#[1] 6778
  
#Part 2
#Shoelace Theorem
area <-0# list( #initialising with end since we cant do this part in the for loop


for (i in 1:(length(looper)-1)){ # see that the loop takes no time
  x1 <- looper[[i]][1]
  y1 <- looper[[i]][2]
  x2 <- looper[[i+1]][1]
  y2 <- looper[[i+1]][2]
  
  area<- area %>%append((x1*y2) - (x2*y1)) #determinant - see shoelace and picks theorems
  
  
}

area<-(area %>% unlist()%>%sum()/2) %>% floor()

#Picks Theorem - finding i
i <- area +1 -length(looper)/2

part_2 <- i
part_2
#433  