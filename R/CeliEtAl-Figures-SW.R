here::i_am("R/CeliEtAl-Figures.R")
library(here)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(lubridate)
library(scales)
library(icaeDesign)
source(here("R/default_definitions.R"))

# Figure 3 -----
# Caption	Figure 3. 
#  Italian and Visegrád countries value added contained in German exports  Transport equipment(a) – 1996-2018
# Source:	authors’ elaboration on OECD TiVA database
# Note	(a) Value added from Italy and Visegrád countries as a share of total foreign value added contained in German exports in transport equipment

celi_data_f3 <- read_xlsx(
  path = here("data/Celi-Data.xlsx"), 
  sheet = "fig_3", range = "B6:D30") %>% 
  rename("Year" = `...1`) %>% 
  pivot_longer(cols = -"Year", names_to = "country", values_to = "share")

celi_f3 <- ggplot(
  data = celi_data_f3, mapping = aes(x=Year, y=share, color=country, shape=country))+
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_y_continuous(
    labels = label_percent(scale = 1), 
    limits = c(0, 27), 
    expand = expansion()
  ) +
  scale_color_grey() +
  labs(
    title = "Value added contained in German exports", 
    y = "Share of total foreign value added contained\n in German exports in transport equipment",
    caption = "Data: OECD TiVA; authors' own elaboration.") +
  theme_jahrbuch +
  theme(axis.title.y = element_text(size=8))
celi_f3

ggsave(
  plot = celi_f3, 
  width = standard_width, 
  height = standard_heigth, 
  filename = here("output/CeliEtAl-Figure3-SW.pdf"))

