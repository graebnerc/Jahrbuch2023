standard_width <- 5
standard_heigth <- 3
lightgray_euf <- "#6F6F6F32"

theme_jahrbuch <- theme_bw() + 
  theme(
    axis.text = element_text(size=8),
    axis.title = element_text(size=9),
    axis.title.x = element_blank(),
    axis.line = element_line(colour = "#6F6F6F"),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(
      colour = rgb(t(col2rgb("#6F6F6F")), 
                   maxColorValue = 255, alpha = 100),
      linewidth = 0.25),
    panel.grid.major.x = element_line(
      colour = lightgray_euf),
    panel.grid.minor.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )