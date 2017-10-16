library(gbm)
library(MASS)
library(caret)
library(dplyr)
library(tidyr)
fitControl <- trainControl(method = "repeatedcv", 
                           number = 5,
                           repeats = 5,
                           classProbs = T,
                           summaryFunction = twoClassSummary)

gbmFit <- train(class~., data = biopsy[,-1],
                trControl = fitControl,
                na.action = na.omit,
                method = "gbm",
                metric = "ROC")

q.set <- seq(0.2, 1, 0.2)
feature.quantiles <- data.frame(apply(biopsy[,c(-1,-11 )], 2,
                                      quantile, 
                                      probs = q.set,
                                      na.rm = T))

plot.df <- NULL
# perform sensitivity analysis on all variables
# (leave out the ID and outcome)
for(i in names(biopsy[,c(-1,-11)])){
  # quantile value dictates color
  # get range of i and predic for all values
  # in the range
  var.range <- range(biopsy[,i], na.rm = T)
  possible.vals <- seq(var.range[1], var.range[2])
  response <- matrix(0, nrow = nrow(feature.quantiles),
                     ncol = length(possible.vals) + 1, 
                     dimnames = list(NULL, c("Quantile", possible.vals)))
  # first column contains the quantile
  response[,1] <- q.set
  
  # run the model for the selected variable
  # while holding all other variables steady at a quantile
  

for(j in 1:nrow(feature.quantiles)){
  test.set <- apply(feature.quantiles[j,],
                     2,function(x)rep(x,length(possible.vals)))
  test.set[,i] <- possible.vals

response[j,-1] <- predict(gbmFit, newdata = test.set, 
                            type = "prob")[,2]
}

# reshape the response matrix for storage with other results
  temp <-  as.data.frame(response) %>%
    gather(value, response, 2:(length(possible.vals)+1))
  temp$value <- as.numeric(as.character(temp$value))
  # append the name of the explanatory variable tested
  temp$Variable <- i
  # bind all results together into one dataframe 
  plot.df<- rbind(plot.df, temp) 
}

# plot the values ####
palette.offset <- 3
palette <- brewer.pal("GnBu",
                      n=length(q.set)+palette.offset)[
                        seq(from = 3,
                            length.out = length(q.set))]
g <- ggplot(plot.df, aes(x = value, y = response, color = factor(Quantile))) + 
  stat_smooth(size = 1.2, se = F) + labs(x = "Value",
                                         y = "Response",title = "Sensitivity") + 
  scale_colour_manual(name = "Quantile", 
                      values = palette) +
  scale_x_continuous(limits = range(plot.df$value)) +
  facet_grid(.~Variable) + fte_theme()
print(g)
