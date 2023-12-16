library(tidyverse)
library(stringi)
library(readxl)
library(unpivotr)

df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"
df

n <- rowSums(is.na(df)) == ncol(df)
cs <- cumsum(n) + 1
df <- split(df[!n, ], cs[!n])
names(df) <- map(1:length(df), ~paste0("mirrior",.x)) #parsed data as lists of dataframes



lower_horizontal <- function(df,i){
  if(identical(
    df[(nrow(df)-i+1):nrow(df),],
    df[(nrow(df)-i):(nrow(df)-2*i+1),])
    ){(nrow(df)-i)*100}else{0}
}

upper_horizontal <- function(df, i){
  if(identical(df[1:i,], df[(2*i):(i+1),])){(i)*100}
}


col_to_cols <- function(df, col_to_split){
  
  df %>% pluck(col_to_split)%>% str_split("")%>%reduce(rbind)
}

left_vertical <- function(df, j){
  if(identical(df[,1:j], df[,(2*j):(j+1)])){j}else{0}
}

right_vertical <- function(df, j){
  if(identical(
    df[,(ncol(df)-j+1):ncol(df)],
    df[,(ncol(df)-j):(ncol(df)-2*j+1)])
  ){(ncol(df)-j)}else{0}
}

get_reflections <- function(df){
  #Edges
  if(identical(df[1,],df[2,])){return(100)} #top
  if(identical(df[nrow(df)-1,], df[nrow(df),])){return((nrow(df)-1)*100)}#bottom
  
  u_horiz<-map(2:floor(nrow(df)/2), ~upper_horizontal(df,.x))%>%unlist()%>%sum()
  if(u_horiz !=0){return(u_horiz)}
  d_horiz<-map(2:floor(nrow(df)/2), ~lower_horizontal(df,.x))%>%unlist()%>%sum()
  if(d_horiz !=0){return(d_horiz)}
  
  df <- col_to_cols(df,"text")
  
  l_vert <-map(2:floor(ncol(df)/2), ~left_vertical(df,.x))%>%unlist()%>%sum()
  if(l_vert !=0){return(l_vert)}
  r_vert <-map(2:floor(ncol(df)/2), ~right_vertical(df,.x))%>%unlist()%>%sum()
  if(r_vert !=0){return(r_vert)}

  if(identical(df[,1],df[,2])){return(1)} #left
  if(identical(df[,ncol(df)-1], df[,ncol(df)])){return(ncol(df)-1)}#right
  }

map(df, get_reflections)->part_1
part_1

part_1 %>% unlist()%>%sum()
#[1] 30158





#Part 2
#Changing identical to ((df1 %>% unlist()) ==(df2 %>% unlist()))%>%sum() == len(df1 %>% unlist())-1

lower_horizontal <- function(df,i){
  if((
    df[(nrow(df)-i+1):nrow(df),]%>%
    col_to_cols("text")==
    df[(nrow(df)-i):(nrow(df)-2*i+1),]%>%
    col_to_cols("text")
    )%>%
    sum() == 
      length(df[(nrow(df)-i+1):nrow(df),]%>%col_to_cols("text"))-1)
  {(nrow(df)-i)*100}else{0}
}

upper_horizontal <- function(df, i){
  if(
    ((df[1:i,]%>%col_to_cols("text"))==
     (df[(2*i):(i+1),]%>%col_to_cols("text")))%>%
    sum()==length(df[1:i,]%>%col_to_cols("text"))-1){(i)*100}
}


left_vertical <- function(df, j){
  if(
    (df[,1:j]==df[,(2*j):(j+1)])%>%
    sum()==length(df[,1:j])-1){j}else{0}
}

right_vertical <- function(df, j){
  if((df[,(ncol(df)-j+1):ncol(df)]==df[,(ncol(df)-j):(ncol(df)-2*j+1)])%>%
     sum() == length(df[,(ncol(df)-j+1):ncol(df)])-1
  ){(ncol(df)-j)}else{0}
}

get_reflections <- function(df){
  #Edges
  if((
    df[1,]%>%col_to_cols("text")==
    df[2,]%>%col_to_cols("text"))%>%
    sum()==length(df[1,]%>%col_to_cols("text"))-1){return(100)} #top
  if((
    df[nrow(df)-1,]%>%col_to_cols("text")==
    df[nrow(df),]%>%col_to_cols("text"))%>%
    sum()==length(df[nrow(df)-1,]%>%col_to_cols("text"))-1){return((nrow(df)-1)*100)}#bottom
  
  u_horiz<-map(2:floor(nrow(df)/2), ~upper_horizontal(df,.x))%>%unlist()%>%sum()
  if(u_horiz !=0){return(u_horiz)}
  d_horiz<-map(2:floor(nrow(df)/2), ~lower_horizontal(df,.x))%>%unlist()%>%sum()
  if(d_horiz !=0){return(d_horiz)}
  
  df <- col_to_cols(df,"text")
  
  l_vert <-map(2:floor(ncol(df)/2), ~left_vertical(df,.x))%>%unlist()%>%sum()
  if(l_vert !=0){return(l_vert)}
  r_vert <-map(2:floor(ncol(df)/2), ~right_vertical(df,.x))%>%unlist()%>%sum()
  if(r_vert !=0){return(r_vert)}
  
  if((
    df[,1]==
    df[,2])%>%sum()==
    length(df[,1])-1){return(1)} #left
  if((
    df[,ncol(df)-1]==
    df[,ncol(df)])%>%
    sum()==length(df[,ncol(df)-1])-1){return(ncol(df)-1)}#right
}

map(df, get_reflections)->part_2
part_2

part_2 %>% unlist()%>%sum()

