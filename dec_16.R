library(tidyverse)
library(stringi)
library(readxl)
library(unpivotr)
library(janitor)
library(diffdf)
library(memoise)

df<-read_csv("C:/Users/RJHar/Documents/Advent of Code/Dec_16_Data.csv")$x|>as.tibble()
#df <- clipr::read_clip()|>as_tibble()
names(df) <- "text"
# explode character strings into list and convert it to a matrix
col_to_cols <- function(df, col_to_split=NULL){
  if(is.null(col_to_split)){col_to_split <- df |> names()}
  df %>% pluck(col_to_split)%>% str_split("")%>%reduce(rbind)
}

xymap <-df%>%col_to_cols()%>%as_cells()

l_wall <-xymap |> filter(col ==1)
r_wall <- xymap |> filter(col ==max(col))
t_wall <- xymap |> filter(row ==1)
b_wall <- xymap |> filter(row == max(row))

#hack to get ifelse to return output of length greater than input. 
ud <- c("up_down","down_up")
lr <- c("left_right", "right_left")

objects <- xymap |>filter(chr != ".")

# Start dir --> end dir
hittable <- reduce(list(objects, l_wall,r_wall,t_wall,b_wall),rbind)|>unique()|>
  mutate(
    left_right = ifelse(chr == "/","down_up",ifelse(chr == "\\","up_down",ifelse(chr=="|",list(ud),NA))),
    right_left = ifelse(chr == "\\","down_up",ifelse(chr == "/","up_down",ifelse(chr=="|",list(ud),NA))),
    up_down = ifelse(chr == "/","right_left",ifelse(chr == "\\","left_right",ifelse(chr=="-",list(lr),NA))),
    down_up = ifelse(chr == "\\","right_left",ifelse(chr == "/","left_right",ifelse(chr=="-",list(lr),NA))),
    seen=0
    )


move <- function(cur_row,cur_col,direction){
  hittable <-hittable|>mutate(from_row = cur_row,from_col= cur_col)|>select(seen,from_row,from_col,row,col,direction=sym(direction))
  if(direction == "left_right") {return(hittable |>filter((row == cur_row & col >  cur_col & !is.na(direction))|(row == cur_row  & col ==max(col) & is.na(direction)))|>filter(col==min(col))|>unnest(direction))}
  if(direction == "right_left") {return(hittable |>filter((row == cur_row & col <  cur_col & !is.na(direction))|(row == cur_row  & col ==min(col) & is.na(direction)))|>filter(col==max(col))|>unnest(direction))}
  if(direction == "up_down")    {return(hittable |>filter((row >  cur_row & col == cur_col & !is.na(direction))|(row == max(row) & col == cur_col & is.na(direction)))|>filter(row==min(row))|>unnest(direction))}
  if(direction == "down_up")    {return(hittable |>filter((row <  cur_row & col == cur_col & !is.na(direction))|(row == min(row) & col == cur_col & is.na(direction)))|>filter(row==max(row))|>unnest(direction))}
}






#Need to be careful of two beams colliding, causing the same result output and detected as duplicates, without actually being seen first!
recursive_add <- function(result){
  visited <- result |>filter(seen == 1)|>select(row,col,direction)|>unique()
  to_visit <- result |>filter(seen ==0, !is.na(direction))|>select(row,col,direction)|>anti_join(visited)
  if(nrow(to_visit)==0){return(result)}
  result<-result |>mutate(seen=1)
  to_visit <-to_visit |>split(rownames(to_visit))|>map(~move(.x$row,.x$col,.x$direction))|>reduce(rbind,.init = NULL)
  out<-rbind(result,to_visit)
  recursive_add(out)
}


heat_cells <- function(result){
  result |>mutate(row_hit = map2(from_row, row, ~seq(.x,.y)),
                  col_hit = map2(from_col, col, ~seq(.x,.y)))|>
    select(row_hit, col_hit)|>
    unnest(cols = everything())|>unique()|>tail(-1)#|>nrow()
}

#part_1
heat_cells(move(1,0,"left_right")|>recursive_add())->answer
answer|>nrow()
options(error=recover)


#part_2
edges <- tibble(row = c(rep(0,nrow(t_wall)),rep(nrow(b_wall)+1,nrow(b_wall)),c(1:nrow(t_wall)),c(1:nrow(r_wall))),
       col = c(c(1:nrow(t_wall)),c(1:nrow(b_wall)),rep(0,nrow(l_wall)),rep(nrow(r_wall)+1,nrow(r_wall))),direction = c(rep("up_down",nrow(t_wall)),rep("down_up",nrow(t_wall)),rep("left_right",nrow(l_wall)),rep("right_left",nrow(l_wall))))

part_2 <- edges |>mutate(id = row_number(), answer = pmap(list(row, col,direction,id),function(x,y,z,a){print(a);heat_cells(recursive_add(move(x, y,z)))}))
part_2$answer|>lapply(nrow)|>unlist()|>max()

