library(factoextra)
library(datasets)

df <- scale(USArrests)
res.hk <-hkmeans(df, 4)

# Elements returned by hkmeans()
names(res.hk)

# Print the results
res.hk

# Visualize the tree
fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

# Visualize the hkmeans final clusters
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())
