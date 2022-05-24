# install.packages("rstatix")
# install.packages("palmerpenguins")


# Libraries needed --------------------------------------------------------


library(tidyverse)
library(rstatix)
library(palmerpenguins)


# Sample Correlation Testing ---------------------------------------------

# select Adelie penguins
adelie <- penguins %>% 
  filter(species == "Adelie") %>% 
  select(c(2, 3, 6)) %>% # keep only relevant data
  drop_na()

# Pearson
adelie %>% 
  cor_test(bill_length_mm, body_mass_g)

# Spearman
adelie %>% 
  cor_test(bill_length_mm, body_mass_g, 
           method = "spearman")

# Directional Testing (one-tailed)
adelie %>% 
  cor_test(bill_length_mm, body_mass_g, 
           alternative = "greater")

# Correlation Matrix ------------------------------------------------------

penguins %>% 
  drop_na %>%
  cor_mat(penguins %>% select(where(is.numeric)) %>% names)

penguins %>% 
  drop_na %>%
  cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  cor_reorder

# get p-values

penguins %>% 
  drop_na %>%
  cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  cor_reorder %>%
  cor_get_pval

# coeffiecents and pvalues

penguins %>% 
  drop_na %>%
  cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  cor_reorder %>%
  cor_mark_significant

# diamonds data (penguins are much more interesting!)

diamonds %>% 
  drop_na %>%
  cor_mat(c("x","y","z","price","carat")) %>%
  cor_mark_significant

# back to penguins

penguins %>% 
  drop_na %>%
  cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  cor_gather

# visualise

penguins %>% 
  drop_na %>%
  cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  cor_plot()


penguins %>% 
  drop_na %>%
  cor_mat(penguins %>% select(where(is.numeric)) %>% names) %>%
  cor_gather %>% 
  ggplot(aes(var1,var2)) +
  geom_tile(aes(fill=p < .01), col = "white") +
  geom_text(aes(label = cor), col = "white", show.legend = F) +
  theme_minimal()


# chi-squared -------------------------------------------------------------

table(diamonds$cut,diamonds$color)

ggplot(diamonds) +
  aes(x = cut, fill = color) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal() +
  coord_flip()

ggplot(diamonds) +
  aes(x = cut, fill = color) +
  geom_bar(aes(y = ..prop..)) +
  scale_fill_hue() +
  theme_minimal() +
  coord_flip()

chisq_test(diamonds$cut,diamonds$color)

