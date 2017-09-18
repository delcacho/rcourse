# Create example data
set.seed(123)
# create nominal variable
nom <- factor(rep(letters[1:3], each=10))
# create numeric variables
vars <- as.matrix(replicate(17, rnorm(30)))
df <- data.frame(nom, vars)

library(cluster)
daisy.mat <- as.matrix(daisy(df, metric="gower"))

library(StatMatch)
gower.mat <- gower.dist(df)

# you can look directly to see the numbers are the same
head(daisy.mat, 3)
head(gower.mat, 3)

