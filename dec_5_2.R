library(readxl)
library(tidyverse)
library(intervals)



df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"
df

n <- rowSums(is.na(df)) == ncol(df)
cs <- cumsum(n) + 1
df <- split(df[!n, ], cs[!n])
df


seed_range <- df %>% pluck(1) %>%
  mutate(text= str_extract_all(text, "\\d+"))%>% unnest(text)%>%
  mutate(text = as.numeric(text)) %>%
  mutate(pair = (row_number()-1) %/% 2) %>% 
  rename(seed = text)%>%group_by(pair)%>%
  mutate(pos = c("start", "range")) %>%
  spread(pos, seed) %>% 
  mutate(end = start + range -1)%>%
  select(-range)


unwrap_df <- function(list_df){
  list_df<- list_df[-1]
  names(list_df) <- map(list_df, ~.x %>% head(1) %>% pluck(1))
  list_df <- map2(
    .x=list_df, .y = names(list_df),
    ~.x %>% tail(-1) %>%
      mutate(
        quantity = str_extract(text, "\\d+$")%>% as.numeric(),
        destination_start = str_extract(text, "\\d+")%>% as.numeric(),
        #destination_end = destination_start + quantity -1, #Not needed - this might save time
        source_start = str_remove(text, destination_start%>% as.character()) %>% str_extract( "\\d+")%>% as.numeric(),
        source_end = source_start + quantity -1,
        map_name = .y
      )%>%
      select(-c(text,quantity))#%>%
    #select(-text)%>%
    #rowwise()%>%
    #mutate(range = list(source_start:source_end))%>% 
    #ungroup()%>%
    #unnest(range)%>%
    #add_missing_ranges(min_value = min_value, max_value = max_value)%>%
    #mutate(map_to = destination_start+range - source_start) 
  ) 
  list_df %>% bind_rows()
}

df <- unwrap_df(df)%>%group_by(map_name = consecutive_id(map_name))

range_maps <- df %>% #converting tibble of ranges to tibble of "Intervals"
  group_by(map_name) %>% 
  summarise(ranges = list(Intervals(cbind(source_start-1, source_end+1))))



get_new_ranges <- function(current_ranges, step) {
  # Find the overlapping ranges - adjusting where required
  overlaps <- current_ranges %>%
    crossing(df %>% filter(map_name == i)) %>% #Get every possible combination of map 'i' to map 'i+1' so that we can determine if they overlap
    filter(start <= source_end, end >= source_start) %>% #filter these by 
    mutate(new_start = max(start, source_start) - source_start + destination_start,
           new_end = min(end, source_end) - source_start + destination_start)%>% arrange(new_start) %>% select(new_start, new_end)
  
  names(overlaps)<- c("start", "end")
  
  # Find any non-overlapping ranges - Made much more easy with intervals package <3
  source_range = Intervals(current_ranges[c("start", "end")]%>% arrange(start))
  mapping_range = range_maps$ranges[[step]]
  no_overlap <- interval_difference(source_range, mapping_range)%>% as.data.frame()%>%as_tibble() #MUST GO DF -> TIBBLE For some reason...
  names(no_overlap) <- c("start", "end")
  
 
    bind_rows(overlaps,no_overlap)
}

# Run it with map_numbers 1-7
final_seed_ranges <- purrr::reduce(1:7, join_range, .init = seed_range) #That actually works for once! No more output <<- output

final_seed_ranges %>%
  summarize(min(start)) #Part two - woop woop
