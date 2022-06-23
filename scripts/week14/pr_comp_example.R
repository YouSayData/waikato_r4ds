library(palmerpenguins)
library(tidyverse)

penguins_no_na <- palmerpenguins::penguins %>%
  drop_na %>%
  mutate(id = row_number())
  
penguins_tbl_num <- penguins_no_na %>% 
  select(c(3:6))

pr_penguins <- prcomp(penguins_tbl_num, scale. = T, center = T) 
penguins_no_na$PC1 <- pr_penguins$x[,"PC1"]
penguins_no_na$PC2 <- pr_penguins$x[,"PC2"]

penguins_no_na %>% 
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(col = species)) +
  facet_wrap(~sex)

