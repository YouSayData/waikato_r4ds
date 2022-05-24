library(tidyverse)


# Toy Data ----------------------------------------------------------------

d <- tibble::tribble(
  ~id, ~group,  ~X, ~Z,       ~Rx, ~condition,     ~Y,
  1L,   "Gb", 102, 1L, "Placebo",       "Ca", 584.07,
  2L,   "Ga",  52, 1L, "Placebo",       "Cb", 790.29,
  3L,   "Gb", 134, 2L, "Dose100",       "Ca", 875.76,
  4L,   "Gb", 128, 3L, "Dose100",       "Cb", 848.37,
  5L,   "Ga",  78, 1L, "Dose250",       "Ca", 270.42,
  6L,   "Gb", 150, 2L, "Dose250",       "Cb", 999.87,
  7L,   "Ga",  73, 1L, "Placebo",       "Ca",  364.1,
  8L,   "Ga",  87, 7L, "Placebo",       "Cb", 420.84,
  9L,   "Gb", 115, 6L, "Dose100",       "Ca", 335.78,
  10L,   "Gb", 113, 4L, "Dose100",       "Cb",    627,
  11L,   "Gc", 148, 3L, "Dose250",       "Ca", 607.79,
  12L,   "Gc",  82, 3L, "Dose250",       "Cb", 329.32,
  13L,   "Ga", 139, 1L, "Placebo",       "Ca", 335.56,
  14L,   "Ga",  65, 2L, "Placebo",       "Cb", 669.04,
  15L,   "Gb", 139, 1L, "Dose100",       "Ca", 405.04,
  16L,   "Gc",  96, 1L, "Dose100",       "Cb", 367.15,
  17L,   "Gb",  50, 5L, "Dose250",       "Ca",  27.37,
  18L,   "Gc",  90, 2L, "Dose250",       "Cb", 468.69,
  19L,   "Ga",  90, 2L, "Placebo",       "Ca", 584.67,
  20L,   "Ga", 116, 2L, "Placebo",       "Cb", 277.71,
  21L,   "Gb",  78, 2L, "Dose100",       "Ca", 266.01,
  22L,   "Gb",  60, 1L, "Dose100",       "Cb",   0.04,
  23L,   "Gc", 112, 4L, "Dose250",       "Ca", 593.25,
  24L,   "Ga",  63, 4L, "Dose250",       "Cb", 512.26,
  25L,   "Ga",  89, 1L, "Placebo",       "Ca", 635.57,
  26L,   "Ga",  97, 2L, "Placebo",       "Cb", 468.69,
  27L,   "Gc",  76, 3L, "Dose100",       "Ca", 514.66,
  28L,   "Gb",  83, 1L, "Dose100",       "Cb", 264.87,
  29L,   "Gc",  84, 4L, "Dose250",       "Ca", 220.34,
  30L,   "Gb",  88, 1L, "Dose250",       "Cb", 216.54
)
d$id <- factor(d$id)
d$group <- factor(d$group)
d$Rx <- factor(d$Rx, levels = c("Placebo", "Dose100", "Dose250"))
d$condition <- factor(d$condition)

glimpse(d)


# Produce a model --------------------------------------------------------

d %>% ggplot() + geom_smooth(aes(X, Y))
d %>% ggplot() + geom_violin(aes(group, Y))
d %>% ggplot() + geom_violin(aes(group, X))

m <- lm(Y ~ group + X, data = d)

summary(m)
anova(m)


# Different order produces different anova --------------------------------

anova(lm(Y ~ group + X, data = d))
anova(lm(Y ~ X + group, data = d))


# Sequence of models ------------------------------------------------------

m0 <- lm(Y ~ 1, data = d)
m1 <- lm(Y ~ group, data = d)

anova(m0, m1, m)
