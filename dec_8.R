library(tidyverse)
library(stringi)
library(readxl)
library(pracma) #skipping creating LCM function because I CBA

df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
#df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"
df


instructions <- df[1,]%>% str_extract_all("[A-Z]")%>%pluck(1)

maze <- df%>%tail(-2) %>% mutate(id = str_extract_all(text, "[A-Z]+"))%>%
  separate(id ,map2(1:4, c("list_unwrap","id","L","R"),~paste0(.y))%>%unlist())%>% select(-c(list_unwrap,text))

start <- maze[1,"id"]%>%pull(1)

next_step <- function(start, instructions){
  if((start %>% str_extract("\\D+"))=="ZZZ"){return(start)}
  
  chars <- str_extract(start,"\\D+")
  count <- str_extract(start,"\\d+")%>%as.numeric() +1
  print(paste(count, paste(maze %>% filter(id ==chars), sep = " "), instructions, sep = " ")) #Debugging code
  start <- paste0(maze%>% filter(id == chars)%>%pluck(instructions),count)

}

start <- "AAA0" #You actually start from AAA not the first line..

part_1 <- reduce(instructions, next_step,.init = paste0(start,0))

while (part_1 %>% str_extract("\\D+") !="ZZZ") {
  start<-part_1 
  print(paste0("loop finished at step " , str_extract(start,"\\d+"))) #Not neccessary but heres what the code is kind of doing
  part_1 <- reduce(instructions, next_step,.init = start)
}

answer_part_1 <- part_1 %>% parse_number()
answer_part_1
#[1] 20093





#Part 2 First attempt - map over all at the same time. Issue is we are finding the LCM of the set - and so this could be very large. It might be faster to split individually and then simply find LCM like normal

start_nodes <- maze %>% filter((id %>% str_extract("A$"))=="A")%>%pull("id")
end_nodes <- maze %>% filter((id %>% str_extract("Z$"))=="Z")%>%pull("id")

#Need to vectorize the reduction, this will involve changing the "next_step" function
next_step <- function(start, instructions){
  if(start %>% map(~str_extract(.x, "\\D+"))%>%unlist()%in% 
     end_nodes %>%prod()==1
     ){return(start)}
  
  chars <- str_extract(start,"\\D+")
  count <- str_extract(start[[1]],"\\d+")%>%as.numeric() +1
  start <- map(chars, ~paste0(maze%>% filter(id == .x)%>%pluck(instructions),count))%>%unlist()
  
}

part_2 <- reduce(instructions, next_step,.init = paste0(start_nodes,0))


while (part_2 %>% map(~str_extract(.x, "\\D+"))%>%
                      unlist() %in% end_nodes %>%prod()==0) {
  start<-part_2 
  print(paste0("loop finished at step " , str_extract(start,"\\d+")))
  part_2 <- reduce(instructions, next_step,.init = start)
}

answer_part_2 <- part_2[[1]] %>% parse_number()
answer_part_2

#Part 2 attempt 2 itterating over each start_node individually

start_nodes <- maze %>% filter((id %>% str_extract("A$"))=="A")%>%pull("id")
end_nodes <- maze %>% filter((id %>% str_extract("Z$"))=="Z")%>%pull("id")
results <- tibble(result = rep(0, length(start_nodes)))

next_step <- function(start, instructions){
  if((start %>% str_extract("\\D(?=\\d)")) == "Z" ){return(start)}
  
  chars <- str_extract(start,"\\D+")
  count <- str_extract(start,"\\d+")%>%as.numeric() +1
  print(paste(count, paste(maze %>% filter(id ==chars), sep = " "), instructions, sep = " ")) #Debugging code
  start <- paste0(maze%>% filter(id == chars)%>%pluck(instructions),count)
  
}




for (i in 1:length(start_nodes)){
  start <- start_nodes[[i]]
  
  part_2 <- reduce(instructions, next_step,.init = paste0(start,0))
  
  while (part_2 %>% str_extract("\\D(?=\\d)") !="Z") {
    start<-part_2 
    print(paste0("loop finished at step " , str_extract(start,"\\d+"))) #Not neccessary but heres what the code is kind of doing
    part_2 <- reduce(instructions, next_step,.init = start)
  }
  results[[i,1]]<- part_2 %>% parse_number()
  
}

results <- results %>% pull(result)

answer_part_2 <- reduce(results, Lcm, .init = 1)
answer_part_2
#2.210306e+13
#Oh its big alright
format(answer_part_2, scientific = FALSE)
#"22103062509257"