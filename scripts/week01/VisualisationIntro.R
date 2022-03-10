library(tidyverse)

# Do cars with big engines use more fuel than cars with small engines?

ggplot2::mpg
?mpg
View(mpg)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# ggplot(data = <DATA>) +
# <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

### EXERCISES
# - Run ggplot(data = mpg). What do you see?
# - How many rows are in mpg? How many columns?
# - What does the drv variable describe? Read the help for ?mpg to find out.
# - Make a scatterplot of hwy vs cyl.
# - What happens if you make a scatterplot of class vs drv? Why is the plot not useful?

# solution extra
ggplot(data = mpg) + 
  geom_count(mapping = aes(x = class, y = drv))

# next part, observe outliers

outliers <- (mpg$hwy > 20) & (mpg$displ > 5)
ggplot(mpg, aes(x = displ, y = hwy, colour = outliers)) +
  geom_point()

# aesthetics 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# other options?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

### EXERCISES
# What’s gone wrong with this code? Why are the points not blue?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

# Which variables in mpg are categorical? Which variables are continuous? (Hint: type ?mpg to read the documentation for the dataset). How can you see this information when you run mpg?
# Map a continuous variable to color, size, and shape. How do these aesthetics behave differently for categorical vs. continuous variables?
# What happens if you map the same variable to multiple aesthetics?
# What does the stroke aesthetic do? What shapes does it work with? (Hint: use ?geom_point)
# What happens if you map an aesthetic to something other than a variable name, like aes(colour = displ < 5)? Note, you’ll also need to specify x and y.

# Extra solutions
glimpse(mpg)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 5)

# Facets

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

## GEOMs

# What's the difference?

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# different line-type

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

# groups and legends

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

# Two geoms

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# but avoid code duplication

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# possible to overwrite global settings for individual layer

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = drv)) + 
  geom_smooth(aes(color = drv, linetype = drv))

#change data for geoms
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

### EXERCISES
# - What geom would you use to draw a line chart? A boxplot? A histogram? An area chart?
# - Run this code in your head and predict what the output will look like. Then, run the code in R and check your predictions.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(aes(linetype = drv), se = FALSE)

# - What does show.legend = FALSE mean? What happens if you remove it? 

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

# - What does the se argument to geom_smooth() do?
# - Reproduce the graphs of the exercise slide.

# grid plots

p1 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

p2 <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

library(gridExtra)
grid.arrange(p1, p2, nrow = 2)

## Statistical Transformation

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

diamonds
table(diamonds$cut)

# bar charts, histograms, and frequency polygons bin your data and then plot bin counts, the number of points that fall in each bin.
# smoothers fit a model to your data and then plot predictions from the model.
# boxplots compute a robust summary of the distribution and then display a specially formatted box.
?geom_bar

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

demo <- table(diamonds$cut) %>% as.data.frame %>% as_tibble
demo

ggplot(demo) +
  geom_bar(aes(x = Var1, y = Freq), stat = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

### EXERCISES

# What is the default geom associated with stat_summary()? How could you rewrite the previous plot to use that geom function instead of the stat function?
# What does geom_col() do? How is it different to geom_bar()?
# What variables does stat_smooth() compute? What parameters control its behaviour?
# n our proportion bar chart, we need to set group = 1. Why? In other words what is the problem with these two graphs?

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop..))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))


## Position Adjustments

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# PA "identity"
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# PA "fill"
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# PA "dodge"
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# PA "jitter"

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

# ggplot2 comes with a shorthand for geom_point(position = "jitter"): geom_jitter()


### EXERCISES

# What is the problem with this plot? How could you improve it?
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point()
# What parameters to geom_jitter() control the amount of jittering?
# Compare and contrast geom_jitter() with geom_count()
# What’s the default position adjustment for geom_boxplot()? Create a visualization of the mpg dataset that demonstrates it.


## Coordinate System

# flip
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

#map
# install.packages("maps")
library(maps)
italy <- map_data("italy")

ggplot(italy, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(italy, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# polar

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

### Exercises
# Turn a stacked bar chart into a pie chart using coord_polar().
# What does the plot below tell you about the relationship between city and highway mpg? Why is coord_fixed() important? What does geom_abline() do?
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline() +
  coord_fixed()

## ggplot2

# don't run

# ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(
#    mapping = aes(<MAPPINGS>),
#    stat = <STAT>, 
#    position = <POSITION>
#  ) +
#  <COORDINATE_FUNCTION> +
#  <FACET_FUNCTION>
