library(tidyverse)
library(googlesheets4)

# Import data -------------------------------------------------------------
gs4_deauth()
sheet_address <- "1qTEc40H35oCn3Kf4kaN3suPXNuYYyRGo0UhzLnt5Zs4"
df <- read_sheet(sheet_address)


# Make plot ---------------------------------------------------------------
make_plot <- function(title_string, df) {
  
  color_vec <- c("#2F69FF", "#DC267F", "#FFB000", "#5fc77b")
  
  ggplot(df, aes(x = time_h, y = mass_mg)) +
    geom_smooth(method='lm', se = F, color = "pink", size = 1.5) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 1000),
                       breaks = seq(0, 1000, 200), 
                       labels = seq(0, 1000, 200)) +
    labs(title = title_string,
         x = "Time (hour)",
         y = "Mass (mg)") +
    theme_bw() +
    theme(axis.title = element_text(size = 26, face = 'bold'),
          axis.text = element_text(size = 22, face = 'bold', color = 'black'),
          axis.text.x = element_text(size = 26, face = 'bold', color = 'black'),
          plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
          panel.border = element_blank(),
          axis.line = element_line(size = 1, color = 'black'),
          axis.ticks = element_line(size = 1, color = 'black'),
          axis.ticks.length = unit(8, 'pt'),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
          panel.grid = element_blank(),
          legend.position = 'none',
          strip.background = element_blank(),
          strip.placement = "outside")
  
  ggsave(filename = file.path(getwd(), "plots", paste0(title_string, ".png")),
         device = 'png',
         width = 7,
         height = 6,
         dpi = 400,
         units = 'in')
}

make_plot("Unheated control", df)

# Statistics --------------------------------------------------------------
regression <- lm(mass_mg ~ time_h, data = df)
summary(regression)
coef(regression) # Would take about 27 hours if the rate was constant, which
# it isn't.

# After 2 hours the volume is reduced by 
mean_zero <- mean(df$mass_mg[df$time_h == 0])
mean_one <- mean(df$mass_mg[df$time_h == 1])
mean_two <- mean(df$mass_mg[df$time_h == 2])
mean_three <- mean(df$mass_mg[df$time_h == 3])

100 * (mean_one - mean_zero) / mean_zero # 0th hour -3.1%
100 * (mean_two - mean_one) / mean_one # 1st hour -3.1%
100 * (mean_three - mean_two) / mean_two # 2nd hour -5.4%

100 * (mean_two - mean_zero) / mean_zero # Over 2 hours -6.1%
100 * (mean_three - mean_zero) / mean_zero # Over 3 hours -11.2%

