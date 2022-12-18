library("tidyverse")
library("npreg")
library("truncnorm")
library("pals")
library("oompaBase")

gre_score <- 160
noised_score <- gre_score + rtruncnorm(mean=0, sd=1.5, n = 1, a = -3, b = 3)
score <- noised_score
palette <- warmcool(50) #greenscale(50) parula(50)
col_num <- number2color(score, colors = palette, ncol = 50, equidistant = TRUE, xmin = min(127), xmax = max(173))

x <- 0
y <- 0
z <- 1
df <- tibble(data.frame(cbind(x, y, z)))

gre_color <- ggplot(df, aes(colour = palette)) +
  geom_point(aes(x, y), color = col_num, size = 120, shape = "square") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) + 
  theme_void()
  # + scale_color_gradientn(colours = palette, breaks = seq(0, 50), guide = "colourbar", aesthetics = "colour", name = "Color", position = "bottom")
gre_color 