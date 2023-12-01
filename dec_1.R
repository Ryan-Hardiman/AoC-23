library(tidyverse)
library(here)
library(stringr)
library(readxl)

df<-read_xlsx("PATH",col_names = FALSE, col_types = "text")
names(df) <- "text"

#Part 1

df1<- df |> mutate(num1 = str_extract(string = text, pattern = "[0-9]"),
             num2 = str_extract(string = text, pattern = "[0-9]{1}(?=[a-z]*$)"),
             entered_num = paste0(num1, num2) |> as.numeric())|> select(entered_num) |> sum()

#Part 2

#possible word letters
"one|two|three|four|five|six|seven|eight|nine"

df2<- df |> mutate(num_1 = str_extract(string = text, pattern = "[0-9]{1}|one|two|three|four|five|six|seven|eight|nine"),
             num_2 = str_extract(string = text, pattern = "([0-9]{1}|one|two|three|four|five|six|seven|eight|nine)(?<=[a-z0-9]{1}$)"))



dfnumber_1_df <- tibble(values = rep(1:9,2), num_1 = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", 1:9))
number_2_df <- tibble(values = rep(1:9,2), num_2 = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", 1:9))
left_join(df1, number_1_df, by = c("num_1")) |> left_join( number_2_df, by = c("num_2")) |>mutate(entered_num = paste0(values.x, values.y)|>as.numeric()) |> select(entered_num)|>sum()
