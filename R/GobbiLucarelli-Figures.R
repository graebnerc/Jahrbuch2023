here::i_am("R/GobbiLucarelli-Figures.R")
library(here)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(icaeDesign)
source(here("R/default_definitions.R"))

# Figure 1: Public debt------------------
debt_data <- readxl::read_xls(
  path = here("data/GobbiLucarelli/imf-GeneralGovernmentDebt.xls"), 
  na = "no data", n_max = 8) %>% 
  dplyr::rename(country=`General Government Debt (Percent of GDP)`) %>% 
  dplyr::filter(!is.na(country)) %>% 
  dplyr::mutate(across(
    .cols = -all_of("country"), 
    .fns = as.character)) %>% 
  pivot_longer(
    cols = -"country",
    names_to = "year", 
    values_to = "GovDebt_GDP") %>% 
  dplyr::mutate(
    GovDebt_GDP=as.double(GovDebt_GDP),
    year = as.double(year)) %>% 
  dplyr::filter(country!="Spain")

gobbi_fig1 <- debt_data %>% 
  ggplot(
    data = ., 
    mapping = aes(
      x=year, y=GovDebt_GDP, 
      color=country, group=country)
  ) +
  geom_line(
    data = filter(debt_data, country=="Italy"),
    alpha=0.9, 
    key_glyph = draw_key_rect) +
  geom_line(
    data = filter(debt_data, country!="Italy"),
    alpha=0.25, 
    key_glyph = draw_key_rect) +
  geom_point(size=0.75, alpha=0.5) + 
  geom_vline(xintercept = 1970, color=get_euf_colors("grey")) +
  geom_vline(xintercept = 1979, color=get_euf_colors("grey")) +
  geom_vline(xintercept = 1993, color=get_euf_colors("grey")) +
  geom_vline(xintercept = 2007, color=get_euf_colors("grey")) +
  geom_vline(xintercept = 2019, color=get_euf_colors("grey")) +
  labs(
    title = "Public debt in a historical perspective",
    y = "General Government debt (% of GDP)",
    caption = "Data: IMF.") +
  scale_color_euf(palette = "mixed") +
  scale_x_continuous(
    breaks = seq(1950, 2020, 10)
  ) +
  scale_y_continuous(
    limits = c(0, 160),
    labels = label_percent(scale = 1)) +
  theme_jahrbuch +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

gobbi_fig1

ggsave(plot = gobbi_fig1, 
       filename = here("output/GobbiLucarelli_Figure1.pdf"), 
       width = standard_width, height = standard_heigth)

# Figure 2: Inflation------------------
inflation_data_raw <- readxl::read_xlsx(
  path = here("data/GobbiLucarelli/gobbi-lucarelli-data.xlsx"), 
  range = "Inflation!A18:C80")

inflation_data <- inflation_data_raw %>% 
  rename(
    inflation = `Inflation Rate (%)`, 
    inflation_change = `Annual Change`) %>% 
  dplyr::mutate(
    date = as.double(substr(date, 1, 4)),
    inflation_change = as.double(inflation_change)
  )

head(inflation_data)

gobbi_fig2 <- inflation_data %>% 
  ggplot(
    data = ., 
    mapping = aes(x=date, y=inflation)
  ) +
  geom_line(
    alpha=0.9, key_glyph = draw_key_rect, 
    color=icaeDesign::get_euf_colors("blue") 
  ) + 
  geom_point(
    size=1, alpha=0.95, 
    color=icaeDesign::get_euf_colors("blue")) + 
  labs(
    title = "Inflation rate in Italy",
    y = "Inflation rate",
    caption = "Data: ECB.") +
  scale_x_continuous(
    breaks = seq(1950, 2020, 10)
  ) +
  scale_y_continuous(
    limits = c(-1, 22),
    labels = label_percent(scale = 1)) +
  theme_jahrbuch

gobbi_fig2 

ggsave(plot = gobbi_fig2, 
       filename = here("output/GobbiLucarelli_Figure2.pdf"), 
       width = standard_width, height = standard_heigth)


# Figure 3: GDP growth rate-------
if (FALSE){
  gdp_growth_raw <- WDI::WDI(
    country = "IT", 
    indicator = "NY.GDP.MKTP.KD.ZG", 
    start = 1960, end = 2021)
  gdp_growth_raw %>% 
    select(c("year", "NY.GDP.MKTP.KD.ZG")) %>% 
    write.csv(file = here("data/GobbiLucarelli/gobbi-lucarelli-gdp.csv"))
}

gdp_growth_raw <- read.csv(
  file = here("data/GobbiLucarelli/gobbi-lucarelli-gdp.csv")) %>% 
  rename(growth=`NY.GDP.MKTP.KD.ZG`)

gobbi_fig3 <- gdp_growth_raw %>% 
  ggplot(data = ., mapping=aes(x=year, y=growth)) +
  geom_hline(yintercept = 0, color=get_euf_colors("grey")) +
  geom_line(
    alpha=0.9, key_glyph = draw_key_rect, 
    color=icaeDesign::get_euf_colors("blue") 
  ) + 
  geom_point(
    size=1, alpha=0.95, 
    color=icaeDesign::get_euf_colors("blue")) + 
  labs(
    title = "GDP growth rate in Italy",
    y = "Growth rate in %",
    caption = "Data: World Bank.") +
  # scale_color_euf(palette = "mixed") +
  scale_x_continuous(
    breaks = seq(1950, 2020, 10)
  ) +
  scale_y_continuous(
    # limits = c(-1, 22),
    labels = label_percent(scale = 1)) +
  theme_jahrbuch

gobbi_fig3

ggsave(plot = gobbi_fig3, 
       filename = here("output/GobbiLucarelli_Figure3.pdf"), 
       width = standard_width, height = standard_heigth)


# Figure 4: Wages----------------------
# Inflation adjusted wages 1990-2020. Source: OECD.
wage_data_raw <- readxl::read_xlsx(
  path = here("data/GobbiLucarelli/gobbi-lucarelli-data.xlsx"), 
  range = "Wages!C5:D27") %>% 
  dplyr::rename(country = `...1`, wages = `...2`) %>% 
  dplyr::mutate(sign=ifelse(wages<0, "Negative", "Positive"))

gobbi_fig4 <- wage_data_raw %>% 
  ggplot(data = ., mapping=aes(
    x=reorder(country, -wages), y=wages,
    color=sign, fill=sign)) +
  geom_bar(
    stat = "identity", 
    color="white",# icaeDesign::get_euf_colors("blue"),
    #fill=icaeDesign::get_euf_colors("blue")
  ) + 
  scale_color_euf(palette = "mixed", discrete = TRUE, aesthetics=c("color", "fill")) +
  geom_hline(yintercept = 0, get_euf_colors("grey")) +
  labs(
    title = "Changes in inflation-adjusted wages (1990 - 2020)",
    y = "Wage rate",
    caption = "Data: OECD.") +
  coord_flip() +
  scale_y_continuous(
    labels = label_percent(scale = 1), 
    breaks = seq(0, 250, 50),
    expand = expansion(add = c(0.5, 0))) +
  theme_jahrbuch +
  theme(axis.title.y = element_blank(), legend.position = "none",
        panel.grid.major.y = element_blank())
gobbi_fig4

ggsave(plot = gobbi_fig4, 
       filename = here("output/GobbiLucarelli_Figure4.pdf"), 
       width = standard_width, height = standard_heigth)


# Figure 5: Spreads--------------------
# Spreads between 10 years German Bunds and other countries 10 years public bonds 
spread_data <- readxl::read_xlsx(
  path = here("data/GobbiLucarelli/gobbi-lucarelli-data.xlsx"), 
  range = "Spreads!B271:F535") %>%
  pivot_longer(cols = -"Year", names_to = "Country", values_to = "Spread") %>% 
  dplyr::mutate(
    Month = rep(c(paste0("0", 1:9), 10:12), 88),
    Observation = paste0(Year, "-", Month),
    Observation2 = ym(Observation) )


gobbi_fig5 <- spread_data %>% 
  ggplot(
    data = ., 
    mapping = aes(x=Observation2, y=Spread, color=Country)
  ) +
  geom_line(
    alpha=0.9, key_glyph = draw_key_rect 
  ) + 
  geom_point(
    size=0.5, alpha=0.5) + 
  labs(
    title = "Spread to German 10-year-Bonds",
    y = "10-year-bond yield spread to Germany",
    caption = "Data: ECB.") +
  # scale_x_continuous(
  #   breaks = seq(2000, 2020, 10)
  # ) +
  scale_color_euf(palette = "mixed") +
  scale_y_continuous(
    limits = c(0, 0.3),
    labels = label_percent(scale = 1)) +
  theme_jahrbuch

gobbi_fig5 

ggsave(plot = gobbi_fig5, 
       filename = here("output/GobbiLucarelli_Figure5.pdf"), 
       width = standard_width, height = standard_heigth)

# Figure 6: Primary balance------------
# Primary balance as percentage of GDP. Source: Eurostat
balance_data <- readxl::read_xlsx(
  path = here("data/GobbiLucarelli/gobbi-lucarelli-data.xlsx"), 
  range = "Primary balance!D29:AQ33")%>%
  select(-names(.)[2:8]) %>%
  rename(Country = `Avanzo Primario`) %>%
  pivot_longer(
    cols = -Country, 
    names_to = "Year", 
    values_to = "PrimaryBalance") %>%
  mutate(Year=as.double(Year))

gobbi_fig6 <- balance_data %>% 
  ggplot(
    data = ., 
    mapping = aes(x=Year, y=PrimaryBalance, color=Country, group=Country)) +
  geom_hline(yintercept = 0, color=get_euf_colors("grey")) +
  geom_line(alpha=0.9, key_glyph = draw_key_rect) + geom_point(size=0.75, alpha=0.5) + 
  labs(
    title = "Primary balance as percentage of GDP",
    y = "Primary balance (% of GDP)",
    caption = "Data: Eurostat") +
  scale_color_euf(palette = "mixed") +
  scale_x_continuous(
    breaks = seq(1950, 2020, 5)
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1)) +
  theme_jahrbuch +
  theme(panel.grid.minor = element_blank())

ggsave(plot = gobbi_fig6, 
       filename = here("output/GobbiLucarelli_Figure6.pdf"), 
       width = standard_width, height = standard_heigth)

# Figure 7: Deficits and GDP-------------
# Deficit/GDP dynamic. Source: Eurostat
deficit_data <- readxl::read_xlsx(
  path = here("data/GobbiLucarelli/gobbi-lucarelli-data.xlsx"), 
  range = "Deficit!A10:AH14")%>%
  select(-names(.)[2:3]) %>%
  rename(Country = `...1`) %>%
  pivot_longer(
    cols = -Country, 
    names_to = "Year", 
    values_to = "Deficit") %>%
  mutate(Year=as.double(Year))

gobbi_fig7 <- deficit_data %>% 
  ggplot(
    data = ., 
    mapping = aes(x=Year, y=Deficit, color=Country, group=Country)) +
  geom_hline(yintercept = 0, color=get_euf_colors("grey")) +
  geom_line(alpha=0.9, key_glyph = draw_key_rect) + geom_point(size=0.75, alpha=0.5) + 
  labs(
    title = "National deficits over time",
    y = "National deficit (% of GDP)",
    caption = "Data: Eurostat") +
  scale_color_euf(palette = "mixed") +
  scale_x_continuous(
    breaks = seq(1950, 2020, 5)
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1)) +
  theme_jahrbuch +
  theme(panel.grid.minor = element_blank())

ggsave(plot = gobbi_fig7, 
       filename = here("output/GobbiLucarelli_Figure7.pdf"), 
       width = standard_width, height = standard_heigth)

