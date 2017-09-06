library(plyr)
library(dplyr)
library(ggplot2)

sizes <- c(20, 100, 500)

set.seed(2015)

centers <- data.frame(x = c(1, 4, 6), y = c(5, 0, 6), n = sizes,
                      cluster = factor(1:3))
points <- centers %>% group_by(cluster) %>%
    do(data.frame(x = rnorm(.$n, .$x), y = rnorm(.$n, .$y)))

plot <- ggplot(points, aes(x, y)) + geom_point()
print(plot)
