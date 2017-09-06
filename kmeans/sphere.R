library(plyr)
library(dplyr)
library(ggplot2)

set.seed(2015)

n <- 250
c1 <- data_frame(x = rnorm(n), y = rnorm(n), cluster = 1)
c2 <- data_frame(r = rnorm(n, 5, .25), theta = runif(n, 0, 2 * pi),
                 x = r * cos(theta), y = r * sin(theta), cluster = 2) %>%
    dplyr::select(x, y, cluster)

points1 <- rbind(c1, c2) %>% mutate(cluster = factor(cluster))

plot <- ggplot(points1, aes(x, y)) + geom_point()
print(plot)
