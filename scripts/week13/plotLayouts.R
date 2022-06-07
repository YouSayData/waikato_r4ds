library(tidyverse)
library(patchwork)
library(ggthemes)


# Patchwork --------------------------------------------------------------

# Basic example 

(p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp)))
(p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear)))

p1 + p2

p1 / p2

# by default patchwork uses a grid and tries to fill it
(p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec)))
(p4 <- ggplot(mtcars) + geom_bar(aes(carb)))

p1 + p2 + p3 + p4

# it works a bit like mathematical equations though
# what will this look like?
p1 + p2 + p3 / p4

# and this?
(p1 + p2 + p3) / p4

# you can have more control by adding a layout
p1 + p2 + p3 + p4 + plot_layout(nrow = 3, byrow = FALSE)

# the + is tricky, more control uses |
# while both most likely create the same plot technically + defines a filling operation
# while | switches the column

p1 | (p2 / p3) 
p1 + p2 / p3 


# annotations  ------------------------------------------------------------

(p1 | (p2 / p4)) + 
  plot_annotation(title = 'The surprising story about mtcars')

# leaving space

(p1 | (plot_spacer() / p4)) + 
  plot_annotation(title = 'The surprising story about mtcars')


# beyond the grid ---------------------------------------------------------

# text layout, make rectangles 

layout <- "
##BBBB
AACCDD
##CCDD
"

p1 + p2 + p3 + p4 + 
  plot_layout(design = layout)

# specify layout
layout <- c(
  area(t = 2, l = 1, b = 5, r = 4),
  area(t = 1, l = 3, b = 3, r = 5)
)

p1 + p2 + 
  plot_layout(design = layout)

# inset

p1 + inset_element(p2, left = 0.6, bottom = 0.6, right = 1, top = 1)

# collecting legends ------------------------------------------------------

(p1a <- ggplot(mtcars) + 
  geom_point(aes(mpg, disp, colour = mpg, size = wt)) + 
  ggtitle('Plot 1a'))

(p3a <- ggplot(mtcars) + 
  geom_point(aes(hp, wt, colour = mpg)) + 
  ggtitle('Plot 3'))

p1a | (p2 / p3a)

(p1a | (p2 / p3a)) + plot_layout(guides = 'collect')



# plot assembly -----------------------------------------------------------

# patchwork can connect all graphical objects aka grobs

p1 + gridExtra::tableGrob(mtcars[1:10, c('mpg', 'disp')])

# but only when it starts with a plot
gridExtra::tableGrob(mtcars[1:10, c('mpg', 'disp')]) + p1

# if you want to start with a grob that is not a plot, use wrap_elements()

wrap_elements(gridExtra::tableGrob(mtcars[1:10, c('mpg', 'disp')])) + p1


# What do you think this one does? ----------------------------------------

p1 + p2 + geom_jitter(aes(gear, disp))

# or this one?

patchwork <- p1 + p2
patchwork[[1]] <- patchwork[[1]] + theme_minimal()
patchwork[[2]] <- patchwork[[2]] + theme_clean()
patchwork

# does this surprise you?
patchwork <- p1 + p2
patchwork + theme_clean()

patchwork & theme_clean()


# More annotations --------------------------------------------------------

patchwork <- (p1 + p2) / p3
patchwork + plot_annotation(
  title = 'The surprising truth about mtcars',
  subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  caption = 'Disclaimer: None of these plots are insightful'
)

patchwork + plot_annotation(tag_levels = '1')
patchwork + plot_annotation(tag_levels = 'A')
patchwork + plot_annotation(tag_levels = 'a')
patchwork + plot_annotation(tag_levels = 'I')
patchwork + plot_annotation(tag_levels = 'i')

patchwork[[1]] <- patchwork[[1]] + plot_layout(tag_level = 'new')
patchwork + plot_annotation(tag_levels = c('A', '1'))

# you can style the plot annotations different than the rest

patchwork + plot_annotation(tag_levels = c('A', '1'), tag_prefix = 'Fig. ',
                            tag_sep = '.',
                            title = 'The surprising truth about mtcars',
                            theme = theme(plot.title = element_text(size = 18))) & 
  theme(text = element_text('mono'),
        plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 8, hjust = 0, vjust = 0))


# Using patchwork and cowplot to add a custom legend ----------------------

p1
p3a
p1 + inset_element(cowplot::get_legend(p3a), left = 0.8, bottom = 0.5, right = 1, top = 1) 

mpg
diamonds

ggplot(mpg, aes(hwy, displ)) +
  geom_point(col = "green") +
  geom_point(aes(carat, cut), data = diamonds, col = "red")

args(ggplot)
args(geom_point)

