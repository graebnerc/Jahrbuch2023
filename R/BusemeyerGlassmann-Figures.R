here::i_am("R/BusemeyerGlassmann-Figures.R")
library(here)
library(readxl)
library(tidyr)
library(ggrepel)
library(dplyr)
library(ggpubr)
library(countrycode)
library(ggplot2)
library(scales)
library(icaeDesign)
source(here("R/default_definitions.R"))

# Figure 1------
fig_1_data <- read_xlsx(
  path = here("data/BusemeyerGlassmann/Figure 1 early leavers.xlsx"), 
  sheet = "Sheet 1", range = "A11:B37") %>%
  rename(Region=`GEO (Labels)`, Leavers=`...2`)%>%
  dplyr::mutate(Marked = ifelse(test = Region %in% c(
    "Centro (IT)", "Nord-Est", "Nord-Ovest", "Italy", "Sud", "Isole"), 
    "Yes", "No"))
fig_1_data

Figure_1 <- fig_1_data %>%
  ggplot(
    data = .,
    mapping = aes(
      x=reorder(Region, Leavers), 
      y=Leavers, color=Marked, 
      fill=Marked)
    ) +
  geom_bar(stat = "identity", color="white") +
  scale_color_euf(
    palette = "mixed", 
    aesthetics = c("color", "fill"), 
    reverse = TRUE
    ) +
  labs(
    title = "Early leavers from training and education\n(2023, by NUTS-2 regions)",
    y = "Early leavers in per cent", 
    caption = "Data: Eurostat (2023a)."
  ) +
  scale_y_continuous(
    expand = expansion(0, c(0,3)),
    labels = label_percent(scale = 1)) +
  theme_jahrbuch +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank()
  )

ggsave(
  plot = Figure_1, 
  filename = here("output/BusemeyerGlassmann-Figure1.pdf"),
  width = standard_width, height = standard_heigth)

Figure_1_SW <- Figure_1 + 
  scale_fill_grey(start = 0.8, end = 0.2)

ggsave(
  plot = Figure_1_SW, 
  filename = here("output/BusemeyerGlassmann-Figure1-SW.pdf"),
  width = standard_width, height = standard_heigth)

# Figure 2--------------
fig_2_data <- read_xlsx(
  path = here("data/BusemeyerGlassmann/Figure 2 Vocational education.xlsx"), 
  sheet = "Tabelle1", range = "B6:C33") %>%
  rename(Region=`...1`, VocationalEduc=`...2`) %>%
  dplyr::mutate(
    Marked = ifelse(test = Region %in% c(
      "Centro (IT)", "Nord-Est", "Nord-Ovest", "Italy", "Sud", "Isole"), 
      "Yes", "No"),
    Region = ifelse(Region == "Valle d’Aosta", "Valle d'Aosta", Region))
fig_2_data

Figure_2 <- fig_2_data %>%
  ggplot(
    data = .,
    mapping = aes(
      x=reorder(Region, VocationalEduc), 
      y=VocationalEduc, color=Marked, 
      fill=Marked)
  ) +
  geom_bar(stat = "identity", color="white") +
  scale_color_euf(
    palette = "mixed", 
    aesthetics = c("color", "fill"), 
    reverse = TRUE
  ) +
  labs(
    title = "Share of population with vocational education\n(2023, by NUTS-2 regions)",
    y = "Per cent of the population", 
    caption = "Data: Eurostat (2023b); population comprises people aged 25-64; \nvocational education refers to upper and post-secondary, non tertiary education."
  ) +
  scale_y_continuous(
    expand = expansion(0, c(0,3)),
    labels = label_percent(scale = 1)) +
  theme_jahrbuch +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1), 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank()
  )

ggsave(
  plot = Figure_2, 
  filename = here("output/BusemeyerGlassmann-Figure2.pdf"),
  width = standard_width, height = standard_heigth+0.25)

Figure_2_SW <- Figure_2 + 
  scale_fill_grey(start = 0.8, end = 0.2)

ggsave(
  plot = Figure_2_SW, 
  filename = here("output/BusemeyerGlassmann-Figure2-SW.pdf"),
  width = standard_width, height = standard_heigth++0.25)

# Figure 3--------------
fig_3_data <- read_xlsx(
  path = here("data/BusemeyerGlassmann/Figure 3 Enrolment rates.xlsx"), 
  sheet = "Tabelle1", range = "B6:C28") %>%
  rename(Region=`...1`, VocationalEduc=`...2`) %>%
  dplyr::mutate(
    Marked = ifelse(test = Region %in% c("Italia"), 
      "Yes", "No"),
    Region = ifelse(
      Region == "Valle d’Aosta", "Valle d'Aosta", Region),
    Region = ifelse(
      Region == "Prov. Aut. Bolzano / Bozen", "Bolzano / Bozen", Region)
    )
fig_3_data

Figure_3 <- fig_3_data %>%
  ggplot(
    data = .,
    mapping = aes(
      x=reorder(Region, VocationalEduc), 
      y=VocationalEduc, color=Marked, 
      fill=Marked)
  ) +
  geom_bar(stat = "identity", color="white") +
  scale_color_euf(
    palette = "mixed", 
    aesthetics = c("color", "fill"), 
    reverse = TRUE
  ) +
  labs(
    title = "Enrollment rate in university education\n(2017, by NUTS-2 regions)",
    y = "Enrollment rate in per cent", 
    caption = "Data: Istat (2023b)."
  ) +
  scale_y_continuous(
    expand = expansion(0, c(0,3)),
    labels = label_percent(scale = 1)) +
  theme_jahrbuch +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1), 
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.x = element_blank()
  )

ggsave(
  plot = Figure_3, 
  filename = here("output/BusemeyerGlassmann-Figure3.pdf"),
  width = standard_width, height = standard_heigth)

Figure_3_SW <- Figure_3 + 
  scale_fill_grey(start = 0.8, end = 0.2)

ggsave(
  plot = Figure_3_SW, 
  filename = here("output/BusemeyerGlassmann-Figure3-SW.pdf"),
  width = standard_width, height = standard_heigth)
