library(tidyverse)
library(stringi)
library(readxl)
library(unpivotr)

df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"

#Basic data functions - which are widely re-usable


# explode character strings into list and convert it to a matrix
col_to_cols <- function(df, col_to_split=NULL){
  if(is.null(col_to_split)){col_to_split <- df |> names()}
  
  df %>% pluck(col_to_split)%>% str_split("")%>%reduce(rbind)
}

visualise_data <- function(df_cleaned){ # Used for debugging visually
  
  df_cleaned |> mutate(data_type = "chr") |>rectify()
  
}
###



stationary_rock_id <- function(df, direction= c("ud","lr")){#Pre-allocating direction - as I expect we will need in part 2
  direction <- match.arg(direction)
  if(direction == "ud"){dir_choice <- as.name("col")}else{dir_choice <- as.name("row")}
    
  df |> 
      group_by(!!dir_choice)|>
      mutate(stationary_id = cumsum(map(chr, ~if(.x=="#"){1}else{0})))%>%
      group_by(!!dir_choice, stationary_id)|>
    mutate(distance = row_number()-1)
  
}


add_roll_count <- function(df_with_stationary_id){
  df_with_stationary_id |>
    mutate(hash_count = map_dbl(chr, ~if(.x=="#"){1}else{0})|>max(),
           roll_count = cumsum(map_dbl(chr, ~if(.x=="O"){1}else{0}))|>max()
           )|> 
    mutate(roll_count = 
             pmap_dbl(
               list(roll_count, distance,hash_count,chr),
               function(.x,.y,.z,.ch)
                 ifelse(.ch=="#", 0 , max(.x - .y+.z,0) )
             )
    ) 
  # p-map to turn c(1,2,3,4,4,4,4) into c(4,3,2,1,0,0,0) with handling for if first character is a # (.z component)
}


move_rocks <- function(df_with_roll_count){
  df_with_roll_count|>
    mutate(
      chr = ifelse(
        roll_count>0,
        "O", ifelse(chr == "O",".",chr)
      )
    )
    
}

clean_up <- function(df_with_moved_rocks){
  
  df_with_moved_rocks |>
    ungroup() |>
    select(row,col,chr)
  
}

find_load <- function(df_cleaned){
  df_cleaned|>
    group_by(col)|>
    arrange(desc(row))|>
    mutate(load = row_number())|>
    group_by(row)|>
    mutate(count = cumsum(map(chr, ~if(.x=="O"){1}else{0}))|>max())
}




#Part 1:

#This is a great way figured out how to quickly turn a df of chr strings into a long df of single chr's with row & col values
long_data <- df |>col_to_cols()|> as_cells() |>select(-data_type) 

part_1<- long_data |> 
  stationary_rock_id()|>  #choose direction and add count for stationary points (s.p.)
  add_roll_count()|>      #add count of movable rocks to each group of s.p
  move_rocks()|> 
  clean_up()|>            #return to row col chr                                
  find_load()|>           #puzzle specific counting -> returning puzzle answer |
  select(row, load, count)|>                                                  #|
  unique()|>reframe(out = load * count)|>                                     #|
  pull("out")|>sum()# <--------------------------------------------------------J

part_1
#[1] 108935


#Part 2 - We can add a few minor functions now that we know we are moving up -> left ->down ->right

adjust_for_direction_down_right <- function(df,direction = c("down","right"),revert = FALSE){ 
  direction<-match.arg(direction) 
  if(direction=="down"){
    group_slct <- "col" |> as.name()
    reverse_by <- "row"|> as.name()
    }else{
      group_slct <- "row"|> as.name()
      reverse_by <- "col"|> as.name()
    }
  
  if(revert){
    df |> group_by(!!group_slct)|>arrange(!!reverse_by)|>ungroup()
  }else{
  df |> group_by(!!group_slct)|>arrange(desc(!!reverse_by))|>ungroup()
  }
}

cycle_once <- function(df_to_spin){ # obviously could make a wrapper for direction but this is fine.
  
  df_to_spin |>
    stationary_rock_id()|>
    add_roll_count()|>
    move_rocks()|>
    clean_up()|> #NORTH
    stationary_rock_id("lr")|>
    add_roll_count()|>
    move_rocks()|>
    clean_up()|> #WEST
    adjust_for_direction_down_right("down")|>
    stationary_rock_id()|>
    add_roll_count()|>
    move_rocks()|>
    clean_up()|> #SOUTH
    adjust_for_direction_down_right("right")|>
    stationary_rock_id("lr")|>
    add_roll_count()|>
    move_rocks()|>
    clean_up()|> #EAST
    arrange(col,row)  #fix arrangement for north movement
    
  
}

map_over_prev_cycles <- function(new_item, prev_list,expected_length){
  map_dbl(prev_list, ~(.x ==new_item)|>sum()==expected_length)|>
    sum()>0 #loop over known cycles - if one matches result will be TRUE --> sum >0
}



run_cycles <- function(df){
  #Start initializing
  previous_cycles <- list(df|>pull(chr)) 
  known_cycle<-FALSE
  i <- 0
  length_to_check <- df |> pull(chr)|>length()
  #End initializing
  
  while(i<1000000000 & known_cycle ==FALSE){ #break on either condition failing
  df<- cycle_once(df)
  known_cycle <- map_over_prev_cycles(df|>pull(chr), previous_cycles,length_to_check)
  previous_cycles <- previous_cycles |> append(list(df|>pull(chr)))
  i <- i+1
  df 
  }
 
  #We have found that a cycle exists within the first i 'spins' - we need length of cycle
  cycle_length <- i+1 - which(
    map(
      previous_cycles,
      ~(.x ==(df|>pull(chr)))|>sum()==length_to_check
      )==TRUE
    )[[1]]
  
  
  #start initialize
  remaining_cycles <- (1000000000 - i+1) %% cycle_length
  j<-1
  #end initialize
  
  while(j<remaining_cycles){ # do remaining cycles
  df<- cycle_once(df)
  j <- j+1
  df
  }
  
  df
} 


part_2 <- long_data |>
  run_cycles()|>          #running cycles until we reach a steady state - or hit 1B cycles
  find_load()|>           #puzzle specific counting -> returning puzzle answer |
  select(row, load, count)|>                                                  #|
  unique()|>reframe(out = load * count)|>                                     #|
  pull("out")|>sum()# <--------------------------------------------------------J

part_2
#[1] 100574 low
#[1] 100946 high



