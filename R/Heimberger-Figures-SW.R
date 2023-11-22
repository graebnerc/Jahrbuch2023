here::i_am("R/Heimberger-Figures-SW.R")
library(here)
library(data.table)
library(countrycode)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(lubridate)
library(scales)
library(icaeDesign)
source(here("R/default_definitions.R"))

# Figure 1 --------
fig1_data <- fread(here("data/Heimberger/Figure1_bookchapter.csv")) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = -"year", names_to = "country", values_to = "LaborProductivity"
    ) %>% 
  dplyr::filter(!is.na(LaborProductivity))

fig1 <- fig1_data %>% 
  ggplot(data = ., mapping=aes(
    x=year, y=LaborProductivity, color=country, shape=country)
    ) +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_color_grey() +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Labor productivity", 
    y="GDP per hours worked\n(1970=100%)", 
    caption = "Data: AMECO; own calculations."
  ) + 
  theme_jahrbuch +
  theme()


ggsave(
  plot = fig1, 
  width = standard_width, 
  height = standard_heigth, 
  filename = here("output/Heimberger_Figure1-SW.pdf"))

# Figure 2 --------
fig2_data <- fread(here("data/Heimberger/Figure2-bookchapter.csv")) %>% 
  as_tibble() %>% 
  rename(
    `Italy/Germany`=ITADEU,
    `Italy/Spain`=ITAESP,
    `Italy/France`=ITAFRA
  ) %>% 
  pivot_longer(
    cols = -"year", names_to = "Relation", values_to = "Ratio"
  )

fig2 <- fig2_data %>% 
  ggplot(data = ., mapping=aes(x=year, y=Ratio, color=Relation, shape=Relation)) +
  geom_hline(yintercept = 0, color=get_euf_colors("grey")) +
  geom_vline(xintercept = 2007, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2010, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2020, color=get_euf_colors("grey"), linetype="longdash") +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_color_grey() +
  scale_x_continuous(
    breaks = seq(1960, 2020, 10)
  ) +
  # scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Differences in real GDP per capita", 
    y="Difference in 1000 \nPurchasing Power Standards (PPS)", 
    caption = "Data: AMECO; own calculations. \nNote: The vertical lines indicate the start of the financial crisis in 2007, \nthe Euro Crisis in 2010 and the Covid-19 crisis in 2020, respectively."
  ) + 
  theme_jahrbuch +
  theme()

ggsave(
  plot = fig2, 
  width = standard_width, 
  height = standard_heigth, 
  filename = here("output/Heimberger_Figure2-SW.pdf"))


# Source: AMECO; own calculations. The vertical lines indicate the start of the financial crisis in 2007, the Euro Crisis in 2010 and the Covid-19 crisis in 2020, respectively.

# Figure 3 --------
fig3_data <- fread(here("data/Heimberger/Figure3-bookchapter.csv")) %>% 
  as_tibble()  %>% 
  pivot_longer(
    cols = -"year", names_to = "Country", values_to = "Demand"
  )

fig3 <- fig3_data %>% 
  ggplot(data = ., mapping=aes(x=year, y=Demand, color=Country, shape=Country)) +
  geom_hline(yintercept = 100, color=get_euf_colors("grey")) +
  geom_vline(xintercept = 2007, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2010, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2020, color=get_euf_colors("grey"), linetype="longdash") +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_color_grey() +
  scale_x_continuous(
    breaks = seq(1960, 2020, 10)
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Real domestic demand", 
    y="Real domestic demand index\n(1960=100%)", 
    caption = "Data: AMECO; own calculations. \nNote: The vertical lines indicate the start of the financial crisis in 2007, \nthe Euro Crisis in 2010 and the Covid-19 crisis in 2020, respectively."
  ) + 
  theme_jahrbuch +
  theme()

ggsave(
  plot = fig3, 
  width = standard_width, 
  height = standard_heigth, 
  filename = here("output/Heimberger_Figure3-SW.pdf"))


# Figure 4 --------
fig4_data <- fread(here("data/Heimberger/Figure4-bookchapter.csv")) %>% 
  as_tibble()

fig4 <- fig4_data %>% 
  ggplot(data = .) +
  geom_vline(xintercept = 2007, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2010, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2020, color=get_euf_colors("grey"), linetype="longdash") +
  geom_line(
    show.legend = FALSE, mapping = aes(
      x=V1, y=lrate, 
      color="10-year-government bond yield")
  ) + 
  geom_line(
    show.legend = FALSE, mapping = aes(
      x=V1, y=pdebt/10, 
      color="Public debt to GDP")
  ) + 
  geom_point(size=1.5, mapping = aes(
    x=V1, y=lrate, 
    color="10-year-government bond yield",
    shape="10-year-government bond yield")) +
  geom_point(
    size=1.5, mapping = aes(
      x=V1, y=pdebt/10, 
      color="Public debt to GDP",
      shape="Public debt to GDP")) +
  scale_color_manual(values = c(
    "10-year-government bond yield"="black",
    "Public debt to GDP"="darkgrey")) +
  scale_shape_manual(values = c(
    "10-year-government bond yield"=16,
    "Public debt to GDP"=17)) +
  scale_x_continuous(
    breaks = seq(1960, 2020, 10)
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    sec.axis = sec_axis(
      trans = ~.*10, name="Public debt in % of GDP", 
      labels = label_percent(scale = 1))
  ) +
  labs(
    title = "Long-term government bond yield & public-debt-to-GDP ratio", 
    y="10-year government bond\nyield in %", 
    caption = "Data: Macrohistory database; own calculations. \nNote: The vertical lines indicate the start of the financial crisis in 2007, \nthe Euro Crisis in 2010 and the Covid-19 crisis in 2020, respectively."
  ) + 
  theme_jahrbuch +
  guides(color=guide_legend(nrow = 2, order = 1), shape="none") +
  theme(plot.title = element_text(size=11))
fig4

ggsave(
  plot = fig4, 
  width = standard_width, 
  height = standard_heigth, 
  filename = here("output/Heimberger_Figure4-SW.pdf"))

# Figure 5 --------
fig5_data <- fread(here("data/Heimberger/Figure5-bookchapter.csv")) %>% 
  as_tibble()

fig5 <- fig5_data %>% 
  ggplot(data = ., mapping=aes(x=year, y=pb)) +
  geom_vline(xintercept = 2007, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2010, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2020, color=get_euf_colors("grey"), linetype="longdash") +
  geom_line(key_glyph=draw_key_rect, color="black") + 
  geom_point(size=1.5, color="black") +
  scale_color_grey(palette = "mixed") +
  scale_x_continuous(
    breaks = seq(1960, 2020, 10)
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Primary fiscal balance in Italy", 
    y="Primary fiscal balance in % of GDP", 
    caption = "Source: Cesaratto and Zezza (2019), Istat; calculations by Francesco Zezza. \nNote: The vertical lines indicate the start of the financial crisis in 2007, \nthe Euro Crisis in 2010 and the Covid-19 crisis in 2020, respectively."
  ) + 
  theme_jahrbuch +
  guides(color=guide_legend(nrow = 2)) +
  theme(plot.title = element_text(size=11))

ggsave(
  plot = fig5, 
  width = standard_width, 
  height = standard_heigth, 
  filename = here("output/Heimberger_Figure5-SW.pdf"))


# Figure 6 --------
fig6_data <- fread(here("data/Heimberger/Figure6-bookchapter.csv")) %>% 
  as_tibble() %>% 
  pivot_longer(cols = -"ccode", names_to = "kind", values_to = "value") %>% 
  dplyr::mutate(kind=ifelse(kind=="tax", "Tax", "Spending"))

fig6 <- fig6_data %>% 
  dplyr::mutate(
    ccode=countrycode(ccode, "iso3c", "country.name")
  ) %>% 
  ggplot(data = ., mapping=aes(x=reorder(ccode, -value), y=value, fill=kind, color=kind)) +
  geom_hline(yintercept = 0, color=get_euf_colors("grey")) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_color_grey(aesthetics = c("color", "fill")) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Fiscal consolidation measures motivated by \na government's desire to cut deficits and debt (1992-2009)", 
    y="fiscal consolidation\n in % of GDP", 
    caption = "Source: Devries et al. (2011)."
  ) + 
  theme_jahrbuch +
  guides(
    fill=guide_legend(title = "Type of fiscal consolidation", 
                      title.position = "left"), 
    color=guide_legend(title = "Type of fiscal consolidation", 
                       title.position = "left")
  ) +
  theme(
    legend.title = element_text(size=10), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size=12))
fig6

ggsave(
  plot = fig6, 
  width = standard_width, 
  height = standard_heigth, 
  filename = here("output/Heimberger_Figure6-SW.pdf"))

# Figure 7 --------
## Figure 7a --------
fig7a_data <- fread(here("data/Heimberger/Figure7a-bookchapter.csv")) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = -"year", names_to = "country", values_to = "EPL index"
  )

fig7a <- fig7a_data %>% 
  ggplot(data = ., mapping=aes(x=year, y=`EPL index`, color=country, shape=country)) +
  #geom_hline(yintercept = 100, color=get_euf_colors("grey")) +
  geom_vline(xintercept = 2007, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2010, color=get_euf_colors("grey"), linetype="longdash") +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_color_grey() +
  scale_x_continuous(
    breaks = seq(1960, 2020, 10)
  ) +
  scale_y_continuous(
    limits = c(1, 5),
    labels = seq(1, 5, 1),
    breaks = seq(1, 5, 1)) +
  labs(
    title = "Regular contracts", 
    y="EPL index"
  ) + 
  theme_jahrbuch +
  guides(color=guide_legend(nrow = 1))  +
  theme(axis.text.x = element_text(hjust = 0.7))
fig7a

## Figure 7b --------
fig7b_data <- fread(here("data/Heimberger/Figure7b-bookchapter.csv")) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = -"year", names_to = "country", values_to = "EPL index"
  )

fig7b <- fig7b_data %>% 
  ggplot(data = ., mapping=aes(x=year, y=`EPL index`, color=country, shape=country)) +
  #geom_hline(yintercept = 100, color=get_euf_colors("grey")) +
  geom_vline(xintercept = 2007, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2010, color=get_euf_colors("grey"), linetype="longdash") +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_color_grey() +
  scale_x_continuous(
    breaks = seq(1960, 2020, 10)
  ) +
  scale_y_continuous(
    limits = c(1, 5),
    labels = seq(1, 5, 1),
    breaks = seq(1, 5, 1)) +
  labs(
    title = "Temporary contracts", 
    y="EPL index"
  ) + 
  theme_jahrbuch +
  guides(color=guide_legend(nrow = 1)) +
  theme(axis.text.x = element_text(hjust = 0.7))
fig7b

## Figure 7full --------

fig7 <- ggarrange(
  fig7a, fig7b, ncol = 2, labels = c("A)", "B"), 
  common.legend = TRUE, legend = "bottom")

fig7 <- annotate_figure(
  fig7, 
  top = text_grob(label = "Employment protection legislation", size=14), 
  bottom = text_grob(
    label = "Data: OECD; own calculations. \nNote: The vertical lines indicate the start of the \n financial crisis in 2007 and the Euro Crisis in 2010.", 
    hjust = 0, size = 8)
)

ggsave(
  plot = fig7, 
  width = standard_width*1.1, 
  height = standard_heigth*1.1, 
  filename = here("output/Heimberger_Figure7-SW.pdf"))

# Figure 8 --------
fig8_data <- fread(here("data/Heimberger/Figure8-bookchapter.csv")) %>% 
  as_tibble() %>% 
  rename(
    `Italy/Germany`=ITADEU,
    `Italy/Spain`=ITAESP,
    `Italy/France`=ITAFRA
  ) %>% 
  pivot_longer(
    cols = -"year", names_to = "Relation", values_to = "Ratio"
  )

fig8 <- fig8_data %>% 
  ggplot(data = ., mapping=aes(x=year, y=Ratio, color=Relation, shape=Relation)) +
  geom_hline(yintercept = 0, color=get_euf_colors("grey")) +
  geom_vline(xintercept = 2007, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2010, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2020, color=get_euf_colors("grey"), linetype="longdash") +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_color_grey() +
  scale_x_continuous(
    breaks = seq(1960, 2020, 10)
  ) +
  scale_y_continuous(breaks = seq(-25, 5, 5)) +
  labs(
    title = "Temporary employment in Italy", 
    y="Diff. in share of temp. employment \nin dependent employment (in ppts.)", 
    caption = "
    Source: OECD.\nNote: The vertical lines indicate the start of the financial crisis in 2007, \nthe Euro Crisis in 2010 and the Covid-19 crisis in 2020, respectively."
  ) + 
  theme_jahrbuch +
  theme(axis.title.y = element_text(size=8), panel.grid.minor.y = element_blank())

ggsave(
  plot = fig8, 
  width = standard_width, 
  height = standard_heigth, 
  filename = here("output/Heimberger_Figure8-SW.pdf"))

# Figure 9 --------
fig9_data <- fread(here("data/Heimberger/Figure9-bookchapter.csv")) %>% 
  as_tibble() %>% 
  pivot_longer(
    cols = -"year", names_to = "country", values_to = "RealComp"
  )

fig9 <- fig9_data %>% 
  ggplot(data = ., mapping=aes(x=year, y=RealComp, color=country, shape=country)) +
  geom_vline(xintercept = 2007, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2010, color=get_euf_colors("grey"), linetype="longdash") +
  geom_vline(xintercept = 2020, color=get_euf_colors("grey"), linetype="longdash") +
  geom_line(show.legend = FALSE) + geom_point(size=1.5) +
  scale_color_grey() +
  scale_x_continuous(
    breaks = seq(1960, 2020, 10)
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Real compensation per employee", 
    y="Real compensation per employee \n(1960=100%)", 
    caption = "Data: AMECO; own calculations. \nNote: The vertical lines indicate the start of the financial crisis in 2007, \nthe Euro Crisis in 2010 and the Covid-19 crisis in 2020, respectively."
  ) + 
  theme_jahrbuch +
  guides(color=guide_legend(nrow = 1)) +
  theme(plot.title = element_text(size=11))

ggsave(
  plot = fig9, 
  width = standard_width, 
  height = standard_heigth, 
  filename = here("output/Heimberger_Figure9-SW.pdf"))

