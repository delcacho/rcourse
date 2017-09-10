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

plot_kmeans <- function(dat, k) {
    clust <- dat %>% ungroup %>% dplyr::select(x, y) %>% kmeans(k)
    ggplot(augment(clust, dat), aes(x, y)) + geom_point(aes(color = .cluster)) +
        geom_point(aes(x1, x2), data = tidy(clust), size = 10, shape = "x") +
        labs(color = "K-means assignments")
}

plot <- plot_kmeans(points, 2)
print(plot)
