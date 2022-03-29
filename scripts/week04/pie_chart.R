library(tidyverse)
library(ggthemes)


# Sidetrack: Pie Chart ----------------------------------------------------
# If you must...

diamond_percent <- diamonds %>% 
  count(cut) %>%
  rename(count = n) %>%
  mutate(perc = count / sum(count))

(pie <- ggplot(diamond_percent, aes(x="", y=perc, fill=cut))+
    geom_bar(width = 1, stat = "identity") + 
    coord_polar("y", start=0))

pie + scale_fill_grey() + theme_minimal()

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

pie + 
  scale_fill_viridis(discrete=TRUE) + 
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = paste0(round(perc, 2) * 100, "%")), 
            position = position_stack(vjust = 0.6), 
            color = "white", fontface='bold')