here::i_am("R/CirilloReljic-Figures.R")
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

# Figure 2-----------------------------
# 
# Source: 
# Note: Figures refer to population 15 to 64 years if not otherwise specified.
fig2n_data_eu <- read_xlsx(here("data/ReljicCirillo/Figure2-New.xlsx"), range = "A1:E5", )
fig2n_data_ita <- read_xlsx(here("data/ReljicCirillo/Figure2-New.xlsx"), range = "A6:E10")
fig2n_data_south <- read_xlsx(here("data/ReljicCirillo/Figure2-New.xlsx"), range = "A11:E15")
fig2n_data <- rbind(fig2n_data_eu, fig2n_data_ita, fig2n_data_south) %>% 
  mutate(`2021`=gsub(",", "\\.", `2021`)) %>% 
  dplyr::mutate(across(contains("0"), .fns = as.double)) %>% 
  pivot_longer(cols = contains("0"), names_to = "year", values_to = "value") %>% 
  dplyr::mutate(year=as.double(year)) %>% 
  dplyr::mutate(Variable_ = case_match(
    Variable,
    "Employment rate"~"Employment\n rate", 
    "Female employment rate"~"Female\n employment rate",
    "Long-term unemployment rate"~"Long-term\n unemployment rate",
    "Youth unemployment rate, 15-29"~"Youth\n unemployment rate", 
    .default = Variable
  ))

fig2 <- ggplot(data = fig2n_data, 
       aes(x=year, y=value, fill=Region, color=Region, shape=Region)
       ) +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_continuous(breaks = c(2004, 2008, 2021)) +
  facet_wrap(~Variable_, ncol = 4, scales = "free_y") +
  scale_color_grey() +
  labs(title = "Selected labour market indicators",
       caption = paste0(
         "Source: Authors' elaboration based on Eurostat and Istat data.\n",
         "Note: Youth unemployment refers to population 15-29 years; \nthe remaining indicators to population 15 to 64 years.")
       ) +
  theme_jahrbuch +
  theme(
    strip.background = element_rect(fill = "white", colour = "white"),
    axis.title = element_blank(), axis.text.x = element_text(
      angle = 40, vjust = 1, hjust = 1.0))

ggsave(plot = fig2, 
       filename = here("output/CirilloReljic_Figure2-SW.pdf"), 
       width = standard_width*1.2, height = standard_heigth)

# Figure 3-----------------------------
# Evolution of value added and hours worked by macro regions, 2008-2020 (base 2008=100) 
# Source: Authors’ elaboration based on national accounts data (ISTAT)
fig2_data <- read_xlsx(here("data/ReljicCirillo/Figure 2.xlsx")) %>% 
  pivot_longer(
    cols = -"year", names_sep = "_", 
    names_to = c("Kind", "Region"),
    values_to = "value") %>% 
  dplyr::mutate(Kind = ifelse(Kind=="VA", "Value Added", "Hours Worked"))
head(fig2_data)

fig2_a <- fig2_data %>% 
  filter(Kind == "Value Added") %>% 
  ggplot(
  data = ., aes(x=year, y=value, color=Region, shape=Region)
  ) +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_color_grey() +
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(title = "Value Added") +
  theme_jahrbuch +
  theme(
    axis.title.y = element_blank()# ,
    # strip.background = element_rect(fill = "white", color = "white"),
    # strip.text = element_text(face = "bold", size = 10)
    )
fig2_a

fig2_b <- fig2_data %>% 
  filter(Kind == "Hours Worked") %>% 
  ggplot(
    data = ., aes(x=year, y=value, color=Region, shape=Region)
  ) +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_color_grey() +
  scale_x_continuous(breaks = seq(2008, 2020, 2)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(title = "Hours Worked") +
  theme_jahrbuch +
  theme(
    axis.title.y = element_blank()# ,
    # strip.background = element_rect(fill = "white", color = "white"),
    # strip.text = element_text(face = "bold", size = 10)
  )
fig2_b

fig_2 <- ggpubr::ggarrange(
  fig2_a, fig2_b, ncol = 2, 
  common.legend = TRUE, 
  legend = "bottom", labels = c("a)", "b)"))
fig_2

fig_2 <- ggpubr::annotate_figure(
  fig_2, bottom = text_grob(
    label = "Source: Authors' elaboration based on national accounts data (ISTAT).", 
    hjust = 0.04, size = 9), 
  top = text_grob(
    label = "Evolution of value added and hours worked by macro regions", 
    face = "bold", size=14))
fig_2

ggsave(plot = fig_2, 
       filename = here("output/CirilloReljic_Figure3-SW.pdf"), 
       width = standard_width*1.5, height = standard_heigth)

# Figure 4-----------------------------
# Medium-high and high-tech manufacturing share by nuts, 2008 vs. 2019
# Source: Authors’ elaboration based on the EU LFS
fig3_data <- read_xlsx(here("data/ReljicCirillo/Figure 3.xlsx")) %>% 
  rename(
    `High-tech`=qht,
    `Low-tech`=qlt,
    KIS=qkis,
    LKIS=qlkis
  ) %>% 
  pivot_longer(cols = -all_of(c("year", "country", "region_group"))) %>% 
  dplyr::mutate(
    region_group = ordered(region_group, levels=c("South","Central", "Northeast", "Northwest")),
    name=ordered(name, levels=c("High-tech", "Low-tech", "KIS", "LKIS"))
  )

f3_a <- fig3_data %>% 
  filter(year==2008) %>% 
  ggplot(aes(x=region_group, value, y=value, fill=name, color=name)) +
  geom_bar(stat = "identity") +
  labs(title = "2008") +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1), 
    expand = expansion(add = c(0, 2))
    ) +
  coord_flip() +
  scale_fill_grey(aesthetics=c("fill", "color")) +
  theme_jahrbuch +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

f3_b <- fig3_data %>% 
  filter(year==2019) %>% 
  ggplot(aes(x=region_group, y=value, fill=name, color=name)) +
  geom_bar(stat = "identity") +
  labs(title = "2019") +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1), 
    expand = expansion(add = c(0, 2))
  ) +
  coord_flip() +
  scale_fill_grey(aesthetics=c("fill", "color")) +
  theme_jahrbuch +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  
fig3 <- ggarrange(
  f3_a, f3_b, ncol = 2,
  common.legend = TRUE, 
  legend = "bottom", labels = c("a)", "b)"))

fig3 <- annotate_figure(
  fig3, 
  top = text_grob(
    label = "Medium-high and high-tech manufacturing share",
    face = "bold", size=14), 
  bottom = text_grob(
    label = "Authors' elaboration based on the EU LFS",
    hjust = 0.0, size = 9)
)

ggsave(plot = fig3, 
       filename = here("output/CirilloReljic_Figure4-SW.pdf"), 
       width = standard_width*1.5, height = standard_heigth)

# Figure 5-----------------------------
# Change in employment growth and occupational composition, 2012-2020
# Source: Authors’ elaboration based on the EU LFS
# Note: Figure 4a represents the average annual growth rates in employment 
#  for each occupational group, while Figure 4b refers to the changes in 
#  occupational structure. 
fig4_data <- read_xlsx(here("data/ReljicCirillo/Figure 4a and 4b.xlsx")) %>% 
  rename(
    delta_manager=deltaqmanager,
    delta_clerk=deltaqclerk,
    delta_craft=deltaqcraft,
    delta_manual=deltaqmanual
  ) %>% 
  pivot_longer(cols = -all_of(c("country",  "year", "region_group", "id")),
               names_sep = "_", 
               names_to = c("Kind", "Group")) %>% 
  dplyr::mutate(
    Group = case_match(
      Group,
      "managers" ~ "Managers",
      "total" ~ "Total EMP",
      "clerks" ~ "Clerks",
      "crafts" ~ "Crafts", 
      "manual" ~ "Manual workers",
      .default = Group)
  ) %>% 
  dplyr::mutate(
    region_group = ordered(region_group, levels=c("Central", "Northeast","Northwest", "South")),
    Group=ordered(Group, levels=c("Managers", "Clerks", "Crafts", "Manual workers", "Total EMP"))
  )
  
fig4_data_bar <- dplyr::filter(fig4_data, Kind == "cagr", Group!="Total EMP") %>% 
  dplyr::mutate(Group=ordered(
    Group, levels = c("Managers", "Clerks", "Crafts", "Manual workers")))

fig4_data_line <- dplyr::filter(fig4_data, Kind == "cagr", Group=="Total EMP") %>% 
  dplyr::mutate(Group = as.character(Group))

fig4_a <- ggplot() +
  geom_bar(
    data = fig4_data_bar, 
    mapping = aes(x=region_group, y=value, fill=Group), 
    color=NA, stat = "identity", position = position_dodge2()
    ) + 
  geom_line(
    data=fig4_data_line, key_glyph=draw_key_abline,
    mapping = aes(x=region_group, y=value, color=Group, group=country)
    ) +
  geom_point(
    data=fig4_data_line, key_glyph=draw_key_abline,
    mapping = aes(x=region_group, y=value, color=Group)
  ) +
  scale_fill_grey() +
  scale_color_manual(values = c("Total EMP"="black")) +
  labs(title = "Average annual growth rate", y = "Avg. annual growth rate") +
  geom_hline(color = get_euf_colors("grey"), yintercept = 0) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  theme_jahrbuch +
  guides(fill=guide_legend(order=2, ncol = 4), 
         color=guide_legend(order=1, ncol = 1)) +
  theme(legend.box="horizontal", panel.grid.minor.y = element_blank())
fig4_a


fig4b_data <- read_xlsx(here("data/ReljicCirillo/Figure 4b.xlsx")) %>% 
  rename(
    Managers=deltaqmanager,
    Clerks=deltaqclerk,
    Crafts=deltaqcraft,
    `Manual workers`=deltaqmanual
  ) %>% 
  pivot_longer(
    cols = -c("country", "region_group"), 
    names_to = "Group", values_to = "value") %>% 
  dplyr::mutate(Group=ordered(
    Group, levels = c("Managers", "Clerks", "Crafts", "Manual workers")))

fig4_b <- fig4b_data %>% 
  #dplyr::filter(Kind == "cagr") %>% 
  ggplot(data=., aes(x=region_group, y=value, fill=Group, color=Group)) +
  geom_bar(stat = "identity", position = position_dodge2()) + 
  scale_fill_grey(aesthetics = c("color", "fill")) +
  labs(title = "Change in occupational shares", y = "Percentage points") +
  geom_hline(color = get_euf_colors("grey"), yintercept = 0) +
  theme_jahrbuch
fig4_b

fig4 <- ggarrange(
  fig4_a, fig4_b, ncol = 2,
  common.legend = TRUE, 
  legend = "bottom", labels = c("a)", "b)"))

fig4 <- annotate_figure(
  fig4, 
  top = text_grob(
    label = "Change in employment growth and occupational composition (2012-2020)",
    face = "bold", size=14), 
  bottom = text_grob(
    label = "Source: Authors' elaboration based on the EU LFS.",
    hjust = 1.3, size = 9, just = "left")
)

fig4 <- annotate_figure(
  fig4, 
  bottom = text_grob(
    "Note: Figure 5a represents the average annual growth rates in employment for each\n occupational group, while Figure 5b refers to the changes in occupational structure.",
    hjust = 0.8, size = 9, just = "left")
)

ggsave(plot = fig4, 
       filename = here("output/CirilloReljic_Figure5-SW.pdf"), 
       width = standard_width*1.5, height = standard_heigth*1.2)

# Figure 7-----------------------------
# Involuntary part-time and transitions from temporary to permanent jobs
# Source: Authors’ elaboration based on Eurostat data
fig6_data <- read_xlsx(here("data/ReljicCirillo/Figure 6.xlsx")) %>% 
  dplyr::mutate(
    variable = case_match(
      variable,
      "involuntary_ptFemales" ~ "Involutary part-time (female)",
      "involuntary_ptMales" ~ "Involutary part-time (male)",
      "transition_temp_permFemales" ~ "Transition temporary to\n permanent jobs (female)",
      "transition_temp_permMales" ~ "Transition temporary to\n permanent jobs (male)",
      .default = variable)
  )

fig6 <- fig6_data %>% 
  ggplot(
    data = ., aes(x=year, y=value, color=variable, shape=variable)
  ) +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_color_grey() +
  scale_x_continuous(breaks = seq(2008, 2022, 2)) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Involuntary part-time and \ntransitions from temporary to permanent jobs", 
    caption = "Source: Eurostat; authors' own computation." ) +
  guides(color=guide_legend(ncol = 1, keyheight = 2)) +
  theme_jahrbuch + theme(axis.title = element_blank(), legend.position = "right")
fig6

ggsave(plot = fig6, 
       filename = here("output/CirilloReljic_Figure7-SW.pdf"), 
       width = standard_width*1.2, height = standard_heigth)

# Figure 8-----------------------------
# Labour market segmentation and risk of poverty
# Source: Authors’ elaboration based on Eurostat data and EU LFS
# Note: Y axis refers to ‘at risk of poverty’ rate and is the same for all four panels 
fig7_data <- read_xlsx(here("data/ReljicCirillo/Figure 7.xlsx"), sheet = "data")

fig7a <- fig7_data %>% 
  ggplot(data = ., aes(x=LRUR_LF, y=AROP, color=group, shape=group)) +
  geom_point(size=1.0) +
  ggrepel::geom_text_repel(
    mapping = aes(label=region_lab), seed = 123, size=2.2, 
    max.overlaps = 15, min.segment.length = 0.2, show.legend = FALSE
    ) +
  scale_fill_grey(aesthetics = c("color", "fill")) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(5, 40)) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Long-term unemployment rate", 
    y = "People at risk of poverty") +
  theme_jahrbuch

fig7b <- fig7_data %>% 
  ggplot(data = ., aes(x=AR, y=AROP, color=group, shape=group)) +
  geom_point(size=1.0) +
  ggrepel::geom_text_repel(
    mapping = aes(label=region_lab), seed = 123, size=2.2, 
    max.overlaps = 15, min.segment.length = 0.2, show.legend = FALSE
  ) +
  scale_fill_grey(aesthetics = c("color", "fill")) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(5, 40)) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Activity rate", 
    y = "People at risk of poverty") +
  theme_jahrbuch

fig7c <- fig7_data %>% 
  ggplot(data = ., aes(x=TEMP_PT, y=AROP, color=group, shape=group)) +
  geom_point(size=1.0) +
  ggrepel::geom_text_repel(
    mapping = aes(label=region_lab), seed = 123, size=2.2, 
    max.overlaps = 15, min.segment.length = 0.2, show.legend = FALSE
  ) +
  scale_fill_grey(aesthetics = c("color", "fill")) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(5, 40)) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Part-time and temporary work", 
    y = "People at risk of poverty") +
  theme_jahrbuch

fig7d <- fig7_data %>% 
  ggplot(data = ., aes(x=QLOW_INT, y=AROP, color=group, shape=group)) +
  geom_point(size=1.0) +
  ggrepel::geom_text_repel(
    mapping = aes(label=region_lab), seed = 123, size=2.2, 
    max.overlaps = 15, min.segment.length = 0.2, show.legend = FALSE
  ) +
  scale_fill_grey(aesthetics = c("color", "fill")) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(5, 40)) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "People in households with very low work intensity", 
    y = "People at risk of poverty") +
  theme_jahrbuch

fig7 <- ggarrange(
  fig7a, fig7b, fig7c, fig7d, 
  ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom",
  labels = paste0(letters[1:4], ")"))

fig7 <- annotate_figure(
  fig7,
  bottom = text_grob(
    label = "Data: Eurostat and EU LFS; authors' own computation.", hjust = 0))

ggsave(plot = fig7, 
       filename = here("output/CirilloReljic_Figure8-SW.pdf"), 
       width = standard_width*1.75, height = standard_heigth*1.75)
