library(tidyverse)
library(stringi)
library(readxl)


df<-read_xlsx(filepath,col_names = FALSE, col_types = "text")
names(df) <- "text"

n <- rowSums(is.na(df)) == ncol(df) #Splitting out the workflows and ratings
cs <- cumsum(n) + 1
df <- split(df[!n, ], cs[!n])
df

workflows <- df[[1]]

ratings <- df[[2]]

get_function_content <- function(workflows, args){
  workflows |> mutate(
    function_name = paste0("rh_",str_extract(text,"^\\w+")),
    ifelses = str_extract_all(text, "\\w+[<=>]\\d+|\\w+,\\w+[<=>]\\d+|\\w+,\\w+"))|>#Get ifelse statements
    unnest(ifelses)|>
    mutate(ifelses = str_split(ifelses, ","))|> #seperate ifelses to core components
    unnest(ifelses)|>
    mutate(ifelses = gsub("([[:alpha:]]+$)", paste0("rh_", "\\1"), ifelses))|> #replace function names with custom prefix
    group_by(function_name)|>
    select(ifelses)|>
    mutate(
      r = row_number(),
      prefix = ifelse(r == 1, "ifelse(", #jiggery-pokery to get correct function type as text (nested ifelses)
                      ifelse(
                        r%%2==1 & r != max(r),
                        ",ifelse(",
                        ifelse(r %%2 ==1 & r == max(r),
                               rep(")",(max(r)-1)/2)|>paste0(collapse = ""),
                               ","))) 
      )|>
    mutate(#if a function - need to tell it what arguments it can take. otherwise we need to assign values by name which is also possible..
      ifelses = ifelse(is.na(str_extract(ifelses,"\\d")), 
                       paste0(ifelses,"(",paste(args,collapse = ","),")"),
                       ifelses)
      )|> 
    mutate(#place together like lego
      prefix_content = ifelse(
        r<max(r),
        paste0(prefix, ifelses),
        paste0(",",ifelses,prefix))
      )|> 
    group_by(function_name)|>
    mutate(function_content = paste0(prefix_content, collapse = ""))|> #Squash them down
    ungroup()|>
    select(function_name,function_content)|>
    unique() #remove duplicates (could be reworked and avoided)
}


get_unique_rating_names <- function(ratings){
  ratings |>
    mutate(list_rating = str_extract_all(text, "[a-z]+"))|>
    unnest(list_rating)|>
    pull(list_rating)|>
    unique()
}

#Return functions
rh_A <- function(...){
  return(sum(...))
}

rh_R <- function(...){return(0)}

make_functions <- function(workflows,ratings){ #double assigning to avoid learning how to properly use env right now.
  fn_name_and_content<-workflows |> 
    get_function_content(get_unique_rating_names(ratings))
  
  map2(fn_name_and_content[,1],fn_name_and_content[,2],
       ~paste(.x , "<<- function(", 
             paste(get_unique_rating_names(ratings = ratings),collapse = ","),
              "){",.y,"}"))%>%unlist()
}



function_factory <-function(fn_text_list){ #code is data, data is code
map(fn_text_list,~parse(text = .x)%>%eval)
}

####
part_1_workflow <- function(workflows, ratings){
  make_functions(workflows, ratings)%>%function_factory()
  
  ratings%>%unlist()%>%map(
    ~eval(parse(text = paste0("rh_in(",str_remove_all(.x,"[{}]"),")"))) #Plug everything through the "in"put function
      )%>%reduce(sum)
}



part_1 <- part_1_workflow(workflows,ratings)
part_1
#[1] 406934

