library(plyr)
library(dplyr)
library(ggplot2)
library(broom)

sizes <- c(20, 100, 500)

set.seed(2015)

centers <- data.frame(x = c(1, 4, 6), y = c(5, 0, 6), n = sizes,
                      cluster = factor(1:3))
points <- centers %>% group_by(cluster) %>%
    do(data.frame(x = rnorm(.$n, .$x), y = rnorm(.$n, .$y)))

points$hclust_assignments <- points %>% dplyr::select(x, y) %>%
    dist() %>% hclust(method = "single") %>%
    cutree(3) %>% factor()

plot <- ggplot(points, aes(x, y, color = hclust_assignments)) + geom_point() +
    labs(color = "hclust assignments")
print(plot)
