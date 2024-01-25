here::i_am("R/Comelli-Figures.R")
library(here)
library(readxl)
library(tidyr)
library(ggrepel)
library(dplyr)
library(countrycode)
library(ggplot2)
library(scales)
library(icaeDesign)
source(here("R/default_definitions.R"))

# Figure 1 -------- 
# Average welfare spending in % of GDP in all OECD countries 1980-2021 
# (Author calculations, OECD SOCX)

fig1 <- readxl::read_xls(
  path = here("data/Comelli/Figure 1.xls")) %>% 
  pivot_longer(cols = -"country", names_to = "Category"
  ) %>% 
  dplyr::filter(country=="OECD", Category!="value90") %>% 
  dplyr::mutate(
    Category = case_match(
      Category, .default = "XXX",
      "value1_2" ~ "Old age",
      "value3" ~ "Incapacity", 
      "value4" ~ "Health", 
      "value5" ~ "Family",
      "value6" ~ "ALM",
      "value7" ~ "Unemployment",
      "value8" ~ "Housing",
      "value9" ~ "Other")
  ) %>% 
  ggplot(mapping = aes(x = reorder(Category, -value), y=value)) +
  geom_bar(stat = "identity", color="white", fill=get_euf_colors("blue")) +
  labs(
    y="% of GDP",
    title = "Average welfare spending of OECD countries\n (1980-2021)",
    caption = "Data: OECD SOCX; author's own calculation.") +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    breaks = seq(0, 8, 1),
    expand = expansion(add = c(0, 1))) +
  theme_jahrbuch +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    )
fig1
ggsave(plot = fig1, 
       filename = here("output/Comelli_Figure-1.pdf"), 
       width = standard_width, height = standard_heigth)
  
# Figure 4 -------- 
# Average total welfare spending as a % GDP 1980-2021 
# (OECD SOCX, Authors calculations)
fig4 <- readxl::read_xls(
  path = here("data/Comelli/Figure 4.xls")) %>% 
  select(c("country", "value90")) %>% 
  mutate(
    marker = ifelse(country=="ITA", "yes", "no"),
    country = countrycode(country, "iso3c", "country.name"), 
    country = ifelse(is.na(country), "OECD", country)) %>% 
  ggplot(mapping = aes(x = reorder(country, -value90), y=value90, fill=marker)) +
  geom_bar(stat = "identity", color="white") +
  labs(
    y="% of GDP",
    title = "Average total welfare spending (1980-2021)",
    caption = "Data: OECD SOCX; author's own calculation.") +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    breaks = seq(0, 30, 5),
    expand = expansion(add = c(0, 1))) +
  scale_fill_euf(palette = "mixed", discrete = TRUE, reverse = TRUE) +
  theme_jahrbuch +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7)
  )
fig4
ggsave(plot = fig4, 
       filename = here("output/Comelli_Figure-4.pdf"), 
       width = standard_width, height = standard_heigth)

# Figure 5 --------  
# Average Italian welfare spending in % of GDP vs average in all OECD countries 1980-2021 
# (Author calculations, OECD SOCX)
fig5 <- readxl::read_xls(
  path = here("data/Comelli/Figure 5.xls")) %>% 
  pivot_longer(cols = -"country", names_to = "Category"
  ) %>% 
  dplyr::filter(country%in%c("OECD", "ITA"), Category!="value90") %>% 
  dplyr::mutate(
    Category = case_match(
      Category, .default = "XXX",
      "value1_2" ~ "Old age",
      "value3" ~ "Incapacity", 
      "value4" ~ "Health", 
      "value5" ~ "Family",
      "value6" ~ "ALM",
      "value7" ~ "Unemployment",
      "value8" ~ "Housing",
      "value9" ~ "Other"),
    country=ifelse(country=="ITA", "Italy", country)
  ) %>% 
  ggplot(mapping = aes(x = reorder(Category, -value), y=value, fill=country)) +
  geom_bar(stat = "identity", color="white", position = position_dodge2()) +
  labs(
    y="% of GDP",
    title = "Average welfare spending in Italy and the OECD\n (1980-2021)",
    caption = "Data: OECD SOCX; author's own calculation.") +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    breaks = seq(0, 14, 2),
    expand = expansion(add = c(0, 1))) +
  scale_fill_euf(palette = "mixed", discrete = TRUE, reverse = TRUE) +
  theme_jahrbuch +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    legend.position = c(0.9, 0.9), legend.background = element_rect(colour = get_euf_colors("grey"))
  )
fig5
ggsave(plot = fig5, 
       filename = here("output/Comelli_Figure-5.pdf"), 
       width = standard_width, height = standard_heigth)

# Figure 6 -------- 
# Incidence rate of absolute poverty in Italy from 2010 to 2020, by age groups 
# (dati.istat.it)
fig6 <- readxl::read_xls(
  path = here("data/Comelli/Figure 6.xls"), skip = 2) %>% 
  pivot_longer(cols = -"Group") %>% 
  ggplot(mapping = aes(x = reorder(Group, -value), y=value, fill=name)) +
  geom_bar(stat = "identity", color="white", position = position_dodge2()) +
  labs(
    y="Incidence rate of absolute poverty ",
    title = "Incidence rate of absolute poverty in Italy",
    caption = "Data: dati.istat.it.") +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    breaks = seq(0, 14, 2),
    expand = expansion(add = c(0, 1))) +
  scale_fill_euf(palette = "mixed", discrete = TRUE, reverse = TRUE) +
  guides(fill = guide_legend(ncol = 2)) +
  theme_jahrbuch +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(colour = get_euf_colors("grey"))
  )
fig6
ggsave(plot = fig6, 
       filename = here("output/Comelli_Figure-6.pdf"), 
       width = standard_width, height = standard_heigth)

# Figure 8 -------- 
# Employment-to-population ratio among women in G7 countries from 2010 to 2022, by country 
# (ILO)

fig8 <- readxl::read_xls(
  path = here("data/Comelli/Figure 8.xls")) %>% 
  pivot_longer(cols = -Year, names_to="country") %>% 
  mutate(
    marker = ifelse(country=="Italy", "yes", "no")) %>% 
  summarise(value=mean(value), .by = c("country", "marker")) %>% 
  ggplot(mapping = aes(x = reorder(country, -value), y=value, fill=marker)) +
  geom_bar(stat = "identity", color="white") +
  labs(
    y="Average 2010-2022",
    title = "Employment-to-population ratio among women\n (2010-2022)",
    caption = "Data: ILO; author's own calculation.") +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    breaks = seq(0, 60, 10),
    expand = expansion(add = c(0, 5))) +
  scale_fill_euf(palette = "mixed", discrete = TRUE, reverse = TRUE) +
  theme_jahrbuch +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), 
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7)
  )
fig8
ggsave(plot = fig8, 
       filename = here("output/Comelli_Figure-8.pdf"), 
       width = standard_width, height = standard_heigth)

# Figure 9 -------- 
# Age-orientation index of welfare spending 
# (Author calculations, OECD SOCX)
fig9_data <- readxl::read_xlsx(
  path = here("data/Comelli/Figure 9.xlsx")) %>% 
  pivot_longer(cols = -time, names_to="country") %>% 
  filter(country!="Grand Total") %>% 
  mutate(time=as.double(time)) 

fig9 <- fig9_data %>% 
  ggplot(aes(x=time, y=value, color=country, group=country)) +
  geom_line() + geom_point() +
  geom_label_repel(
    data=filter(fig9_data, time==2013), 
    mapping = aes(x=time, y=value, color=country, label=country), nudge_x = 5, show.legend = FALSE
    ) +
  scale_color_euf(palette = "mixed") +
  scale_x_continuous(breaks = seq(1981, 2015, 2)) +
  labs(y="Index", 
       title = "Age-orientation index of welfare spending", 
       caption = "Data: OECD SOCX; author's own calculation.") +
  guides(color = guide_legend(ncol = 6)) +
  theme_jahrbuch
fig9
ggsave(plot = fig9, 
       filename = here("output/Comelli_Figure-9.pdf"), 
       width = standard_width+2, height = standard_heigth+1)

