library(tidyverse)
library(stringi)
library(readxl)

df<-read_xlsx("C:/Users/RJHar/Documents/Advent of Code/dec_6.xlsx",col_names = FALSE, col_types = "text")
names(df) <- "text"

df<-df %>% mutate(
  races = str_extract(text, "[a-zA-Z]*"),
  races_n = str_extract_all(text, "\\d+")
)%>%select(-text)
df


no_races = df$races_n[[1]]%>%length()


df<-df%>% 
  separate(races_n ,
           map(
             1:(no_races+1),
               ~ paste0("race_",.x-1)
             )%>%
             unlist()
           )%>% 
  select(-race_0)

df[,-1]<- df[,-1] %>% map(as.numeric)
df


#transposing
df<-as_tibble(cbind(nms = names(df), t(df)))

names(df)<-df[1,]
df<- df %>% tail(-1) %>%
  mutate(Time= Time %>%as.numeric(),
         Distance = Distance %>% as.numeric())%>%
  janitor::clean_names()
#This is a math problem. The distance travelled forms a parabola
#Max distance is found by holding it for half the time (since we dont slow down)

df <- df %>% mutate(
  max = time/2,
  lower_b = (-(-time + sqrt(time^2-4*(distance-0.02)))/(2))%>% ceiling(), #0.02 is an epsilon for correct counting
  upper_b = (-(-time - sqrt(time^2-4*(distance-0.02)))/(2))%>% ceiling(),
  n = upper_b - lower_b
                    )
df

df %>% summarise(part1 = prod(n))

#Part2
df<- tibble(time = 40709879, distance = 215105121471005)%>% mutate(
  max = time/2,
  lower_b = (-(-time + sqrt(time^2-4*(distance-0.02)))/(2))%>% ceiling(), #0.02 is an epsilon for correct counting
  upper_b = (-(-time - sqrt(time^2-4*(distance-0.02)))/(2))%>% ceiling(),
  n = upper_b - lower_b
)
df
