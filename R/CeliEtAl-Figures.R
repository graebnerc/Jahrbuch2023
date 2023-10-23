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

# Figure 1 ------
# Caption	Figure 1. Italy real GDP growth rate in historical perspective (1950-2022)
# Source	AMECO
# Note	vertical lines correspond to, respectively:
#  the unions’ protests of 1963 and 1969; 
#  the lira devaluation of 1992; 
#  the change in fiscal stance of 1995; 
#  the Global Financial Crisis of 2008; 
#  the Sovereign Debt Crisis of 2011; 
#  the Covid Pandemic of 2020.
g_lines <- c(1963, 1969, 1992, 1995, 2008, 2011, 2020)

p1 <- c(1945, 1955) # Reconstruction (1945-1955); 
p2 <- c(1955, 1973) # Golden Age (1955-1973); 
p3 <- c(1973, 1979) # Oil shocks (1973-1979); 
p4 <- c(1979, 1999) # European monetary integration (1979-1999); 
p5 <- c(1999, 2023) # Euro membership (2000-today)

y_lower <- -10.5
y_upper <- 10.5
  
celi_data_f1 <- read_xlsx(
  path = here("data/Celi-Data.xlsx"), 
  sheet = "fig_1", range = "B6:C79") %>% 
  rename("Year" = `...1`) 

annotation_size <- 3
celi_f1 <- ggplot(
  data = celi_data_f1, aes(x=Year, y=GDP)) +
  geom_rect(
    mapping = aes(xmin = 1948, xmax = p1[2], ymin = y_lower, ymax = y_upper),
    fill = "#E65032", alpha = 0.005
    ) +
  geom_rect(
    mapping = aes(xmin = p2[1], xmax = p2[2], ymin = y_lower, ymax = y_upper),
    fill = "#00395B", alpha = 0.005
  ) + 
  geom_rect(
    mapping = aes(xmin = p3[1], xmax = p3[2], ymin = y_lower, ymax = y_upper),
    fill = "#E65032", alpha = 0.005
  ) + 
  geom_rect(
    mapping = aes(xmin = p4[1], xmax = p4[2], ymin = y_lower, ymax = y_upper),
    fill = "#00395B", alpha = 0.005
  ) +
  geom_rect(
    mapping = aes(xmin = p5[1], xmax = p5[2], ymin = y_lower, ymax = y_upper),
    fill = "#E65032", alpha = 0.005
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1), limits = c(y_lower, y_upper), 
    expand = expansion()
    ) +
  scale_x_continuous(
    limits = c(1948, 2023),
    breaks = seq(1950, 2020, 5), expand = expansion()) +
  labs(
    title = "Growth in historical perspective (1950-2023)", 
    y = "Real GDP growth rate",
    caption = "Data: AMECO."
    ) +
  theme_jahrbuch +
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(size=10))

for (ll in g_lines){
  celi_f1 <- celi_f1 +
    geom_vline(color=get_euf_colors("grey"), xintercept = ll)
}

celi_f1 <- celi_f1 +
  annotate(
    "label", fill = "lightgray", 
    x=1956, y=-4, size=annotation_size,
    label= "Reconstruction\n (1945-1955)") + 
  annotate(
    "label", fill = "lightgray", 
    x=1965, y=-8, size=annotation_size,
    label= "Golden Age\n (1955-1973)") + 
  annotate(
    "label", fill = "lightgray", 
    x=1975, y=-4, size=annotation_size,
    label= "Oil shocks\n (1973-1979)") + 
  annotate(
    "label", fill = "lightgray", 
    x=1987, y=-8, size=annotation_size,
    label= "Monetary integration\n (1979-1999)") + 
  annotate(
    "label", fill = "lightgray", 
    x=2010, y=-8, size=annotation_size,
    label= "Euro membership\n (2000-today)") 

celi_f1 <- celi_f1 +
  geom_line(color=get_euf_colors("blue")) + 
  geom_point(color=get_euf_colors("blue")) 
celi_f1

ggsave(
  plot = celi_f1, 
  width = standard_width, 
  height = standard_heigth, 
  filename = here("output/CeliEtAl-Figure1.pdf"))

# Figure 2 ------
# Caption	Figure 2. Germany and Italy: Contribution to real GDP growth. 1980-2022
# Source	AMECO
celi_data_f2_a <- read_xlsx(
  path = here("data/Celi-Data.xlsx"), 
  sheet = "fig_2", range = "B6:G49") %>% 
  rename("Year" = `...1`) %>% 
  dplyr::mutate(country="Germany")

celi_data_f2_b <- read_xlsx(
  path = here("data/Celi-Data.xlsx"), 
  sheet = "fig_2", range = "I6:N49") %>% 
  rename("Year" = `...1`) %>% 
  dplyr::mutate(country="Italy")

celi_data_f2 <- rbind(celi_data_f2_a, celi_data_f2_b)

## Fig 2: Germany-----
celi_data_f2_a_1 <- celi_data_f2_a %>% 
  pivot_longer(cols = -all_of(c("Year", "country")), 
               names_to = "Dimension", 
               values_to = "Contribution")

celi_data_f2_a_1_bar <- filter(
  celi_data_f2_a_1, Dimension != "GDP growth rate")
celi_data_f2_a_1_line <- filter(
  celi_data_f2_a_1, Dimension == "GDP growth rate") %>% 
  select(-Dimension)

celi_f2_a <- ggplot(
  data = celi_data_f2_a_1_bar) +
  geom_bar(
    mapping = aes(x=Year, y=Contribution, fill=Dimension), 
    stat = "identity", color=NA, alpha=0.75) +
  geom_line(
    data = celi_data_f2_a_1_line,
    mapping = aes(x=Year, y=Contribution),
    color = get_euf_colors("grey"), show.legend = FALSE
    ) +
  geom_point(
    data = celi_data_f2_a_1_line,
    mapping = aes(x=Year, y=Contribution),
    color = get_euf_colors("grey"), size=0.5, show.legend = FALSE
  ) +
  geom_hline(yintercept = 0, color=get_euf_colors("grey")) +
  scale_fill_euf(palette = "mixed") +
  labs(title = "Germany", y="Contribution to real GDP growth") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(-10, 7)) +
  theme_jahrbuch
celi_f2_a

## Fig 2: Italy-----
celi_data_f2_b_1 <- celi_data_f2_b %>% 
  pivot_longer(cols = -all_of(c("Year", "country")), 
               names_to = "Dimension", 
               values_to = "Contribution")

celi_data_f2_b_1_bar <- filter(
  celi_data_f2_b_1, Dimension != "GDP growth rate")
celi_data_f2_b_1_line <- filter(
  celi_data_f2_b_1, Dimension == "GDP growth rate") %>% 
  select(-Dimension)

celi_f2_b <- ggplot(
  data = celi_data_f2_b_1_bar) +
  geom_bar(
    mapping = aes(x=Year, y=Contribution, fill=Dimension), 
    stat = "identity", color=NA, alpha=0.75) +
  geom_line(
    data = celi_data_f2_b_1_line,
    mapping = aes(x=Year, y=Contribution),
    color = get_euf_colors("grey"), show.legend = FALSE
  ) +
  geom_point(
    data = celi_data_f2_b_1_line,
    mapping = aes(x=Year, y=Contribution),
    color = get_euf_colors("grey"), size=0.5, show.legend = FALSE
  ) +
  geom_hline(yintercept = 0, color=get_euf_colors("grey")) +
  scale_fill_euf(palette = "mixed") +
  labs(title = "Italy", y="Contribution to real GDP growth") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(-10, 7)) +
  theme_jahrbuch
celi_f2_b

## Fig 2: Full-----
celi_f2 <- ggarrange(
  celi_f2_a, celi_f2_b, ncol = 2, common.legend = TRUE, 
  legend = "bottom", labels = c("a)", "b)"))

celi_f2 <- ggpubr::annotate_figure(
  celi_f2, 
  top = text_grob(label = "Contribution to real GDP growth", size = 14), 
  bottom = text_grob(label = "Data: AMECO.", hjust = -3.0, size = 10))

ggsave(
  plot = celi_f2, 
  width = standard_width*1.5, 
  height = standard_heigth, 
  filename = here("output/CeliEtAl-Figure2.pdf"))
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
  data = celi_data_f3, mapping = aes(x=Year, y=share, color=country))+
  geom_line(key_glyph=draw_key_rect) + geom_point() +
  scale_y_continuous(
    labels = label_percent(scale = 1), 
    limits = c(0, 27), 
    expand = expansion()
    ) +
  scale_color_euf(palette = "mixed") +
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
  filename = here("output/CeliEtAl-Figure3.pdf"))

# Figure 4------
# Caption	Figure 4. Germany and Italy: Financial Balances. 1980-2022
# Source AMECO
celi_data_f4_a <- read_xlsx(
  path = here("data/Celi-Data.xlsx"), 
  sheet = "fig_4", range = "B6:G48") %>% 
  rename("Year" = `...1`) %>% 
  dplyr::mutate(country="Germany")

celi_data_f4_b <- read_xlsx(
  path = here("data/Celi-Data.xlsx"), 
  sheet = "fig_4", range = "I6:N48") %>% 
  rename("Year" = `...1`) %>% 
  dplyr::mutate(country="Italy")

celi_data_f4 <- rbind(celi_data_f4_a, celi_data_f4_b)



## Figure 4a ------

celi_data_f4_a_1 <- celi_data_f4_a %>% 
  pivot_longer(cols = -all_of(c("Year", "country")), 
               names_to = "Dimension", 
               values_to = "Contribution")

celi_data_f4_a_1_bar <- filter(
  celi_data_f4_a_1, !Dimension %in% c("Current Account Balance", "Gov. Deficit", "Private sector"))

celi_data_f4_a_1_line <- filter(
  celi_data_f4_a_1, Dimension %in% c("Current Account Balance", "Gov. Deficit", "Private sector")) 

celi_f4_a <- ggplot(
  data = celi_data_f4_a_1_bar) +
  geom_line(
    data = celi_data_f4_a_1_line,
    mapping = aes(x=Year, y=Contribution, color=Dimension),
    show.legend = TRUE,
    key_glyph=draw_key_rect
  ) +
  geom_point(
    data = celi_data_f4_a_1_line,
    mapping = aes(x=Year, y=Contribution, color=Dimension),
    size=0.5, show.legend = TRUE, 
    key_glyph=draw_key_rect
  ) +
  geom_hline(yintercept = 0, color=get_euf_colors("grey")) +
  scale_fill_euf(palette = "mixed") +
  scale_color_euf(palette = "mixed") +
  labs(title = "Germany", y="Contribution to real GDP growth") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(-10, 20), expand = expansion()) +
  theme_jahrbuch
celi_f4_a

## Figure 4b------
celi_data_f4_b_1 <- celi_data_f4_b %>% 
  pivot_longer(cols = -all_of(c("Year", "country")), 
               names_to = "Dimension", 
               values_to = "Contribution")

celi_data_f4_b_1_bar <- filter(
  celi_data_f4_b_1, !Dimension %in% c("Current Account Balance", "Gov. Deficit", "Private sector"))

celi_data_f4_1_line <- filter(
  celi_data_f4_b_1, Dimension %in% c("Current Account Balance", "Gov. Deficit", "Private sector")) 

celi_f4_b <- ggplot(
  data = celi_data_f4_b_1_bar) +
  # geom_bar(
  #   mapping = aes(x=Year, y=Contribution, fill=Dimension), 
  #   stat = "identity", color=NA, alpha=0.2) +
  geom_line(
    data = celi_data_f4_1_line,
    mapping = aes(x=Year, y=Contribution, color=Dimension),
    show.legend = TRUE,
    key_glyph=draw_key_rect
  ) +
  geom_point(
    data = celi_data_f4_1_line,
    mapping = aes(x=Year, y=Contribution, color=Dimension),
    size=0.5, show.legend = TRUE, 
    key_glyph=draw_key_rect
  ) +
  geom_hline(yintercept = 0, color=get_euf_colors("grey")) +
  scale_fill_euf(palette = "mixed") +
  scale_color_euf(palette = "mixed") +
  labs(title = "Italy", y="Contribution to real GDP growth") +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(-10, 20), expand = expansion()) +
  theme_jahrbuch
celi_f4_b

## Fig 4: Full-----
celi_f4 <- ggarrange(
  celi_f4_a, celi_f4_b, ncol = 2, common.legend = TRUE, 
  legend = "bottom", labels = c("a)", "b)"))

celi_f4 <- ggpubr::annotate_figure(
  celi_f4, 
  top = text_grob(label = "Financial Balances", size = 14), 
  bottom = text_grob(label = "Data: AMECO.", hjust = -4.5, size = 7))

ggsave(
  plot = celi_f4, 
  width = standard_width*1.5, 
  height = standard_heigth, 
  filename = here("output/CeliEtAl-Figure4-Reduced.pdf"))

## Version with bars-----
celi_f4_a_ <- celi_f4_a +
  geom_bar(
    mapping = aes(x=Year, y=Contribution, fill=Dimension),
    stat = "identity", color=NA, alpha=0.4) +
  scale_fill_viridis_d(option = "H") +
  guides(fill=guide_legend(
    order=2, ncol = 2,
    override.aes = list(alpha = 0.4)), 
         color=guide_legend(order=1, ncol = 3)) +
  theme(legend.box="vertical")

celi_f4_b_ <- celi_f4_b +
  geom_bar(
    mapping = aes(x=Year, y=Contribution, fill=Dimension),
    stat = "identity", color=NA, alpha=0.4) +
  scale_fill_viridis_d(option = "H") +
  guides(fill=guide_legend(order=2, ncol = 2), 
         color=guide_legend(order=1, ncol = 3)) +
  theme(legend.box="vertical")

celi_f4_ <- ggarrange(
  celi_f4_a_, celi_f4_b_, ncol = 2, common.legend = TRUE, 
  legend = "bottom", labels = c("a)", "b)"))

celi_f4_ <- ggpubr::annotate_figure(
  celi_f4_, 
  top = text_grob(label = "Financial Balances", size = 14), 
  bottom = text_grob(label = "Data: AMECO.", hjust = -3.0, size = 10))

ggsave(
  plot = celi_f4_, 
  width = standard_width*1.5, 
  height = standard_heigth*1.2, 
  filename = here("output/CeliEtAl-Figure4.pdf"))
