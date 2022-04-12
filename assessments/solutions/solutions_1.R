library(tidyverse)

# plot 1 ------------------------------------------------------------------

diamonds %>% 
  ggplot(aes(log(carat), log(price), col = cut)) + 
  geom_smooth(method = "gam")

# plot 2 ------------------------------------------------------------------

mpg %>%
  ggplot(aes(hwy, class, col = class)) +
  geom_violin(draw_quantiles = .5) +
  facet_wrap(~year, nrow = 2)

# plot 3 ------------------------------------------------------------------

library(ggthemes)

faithfuld %>%
  ggplot(aes(eruptions, waiting, col = density)) +
  geom_tile() +
  scale_color_viridis_c(option = "B") +
  labs(title = "Faithful Eruptions",
       y = "Eruption time (in min)",
       x = "Waiting time (in min)") + 
  theme_clean()
