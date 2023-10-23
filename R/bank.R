here::i_am("R/bank.R")
library(readxl)
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
bank_ita <- read_excel(here("data/bank-italy.xlsx")) %>% 
  pivot_longer(cols = contains("*Q")) %>% 
  select(name, value) %>% 
  mutate(name2 = as.yearqtr(format(name), "%Y *Q %q"))


purrr::map(bank_ita, unique)

bank_ita %>% 
  select(name2, value) %>% 
  ggplot(data = ., mapping = aes(x=name2, y=value)) +
  geom_line()
