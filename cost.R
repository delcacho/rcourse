modelInfo <- list(label = "Cost-Sensitive C5.0",
            library = c("C50", "plyr"),
            loop = function(grid) {     
              loop <- ddply(grid, c("model", "winnow", "costFP","costFN"),
                            function(x) c(trials = max(x$trials)))                 

              submodels <- vector(mode = "list", length = nrow(loop))
              for(i in seq(along = loop$trials))
              {
                index <- which(grid$model == loop$model[i] & 
                                 grid$winnow == loop$winnow[i],
                               grid$costFP[i] == loop$costFP[i],
                               grid$costFN[i] == loop$costFN[i])
                trials <- grid[index, "trials"] 
                submodels[[i]] <- data.frame(trials = trials[trials != loop$trials[i]])
              }     
              list(loop = loop, submodels = submodels)
            },
            type = "Classification",
            parameters = data.frame(parameter = c('trials', 'model', 'winnow', "costFP","costFN"),
                                    class = c("numeric", "character", "logical", "numeric","numeric"),
                                    label = c('# Boosting Iterations', 'Model Type', 'Winnow', "CostFP","CostFN")),
            grid = function(x, y, len = NULL, search = "grid") {
              c5seq <- if(len == 1)  1 else  c(1, 10*((2:min(len, 11)) - 1))
              expand.grid(trials = c5seq, model = c("tree", "rules"), 
                          winnow = c(TRUE, FALSE),
                          costFP = 1:2,
                          costFN = 1:2)
              if(search == "grid") {
                c5seq <- if(len == 1)  1 else  c(1, 10*((2:min(len, 11)) - 1))
                out <- expand.grid(trials = c5seq, model = c("tree", "rules"), 
                                   winnow = c(TRUE, FALSE), costFP = 1:2, costFN = 1:2)
              } else {
                out <- data.frame(trials = sample(1:100, replace = TRUE, size = len),
                                  model = sample(c("tree", "rules"), replace = TRUE, size = len),
                                  winnow = sample(c(TRUE, FALSE), replace = TRUE, size = len),
                                  costFP = runif(len, min = 1, max = 20),
                                  costFN = runif(len, min = 1, max = 20))
              }
              out    
            },
            fit = function(x, y, wts, param, lev, last, classProbs, ...) { 
              theDots <- list(...)

              if(any(names(theDots) == "control"))
              {                           
                theDots$control$winnow <- param$winnow
              } else theDots$control <- C5.0Control(winnow = param$winnow)

              argList <- list(x = x, y = y, weights = wts, trials = param$trials,
                              rules = param$model == "rules")

              cmat <-matrix(c(0, param$costFP, param$costFN, 0), ncol = 2)
              rownames(cmat) <- colnames(cmat) <- levels(y)
              if(any(names(theDots) == "costFP")){
                warning("For 'C5.0Cost', the costs are a tuning parameter")
                theDots$costs <- cmat
              } else argList$costs <- cmat

              argList <- c(argList, theDots)
              do.call("C5.0.default", argList)
            },
            predict = function(modelFit, newdata, submodels = NULL) {
              out <- predict(modelFit, newdata)

              if(!is.null(submodels))
              {
                tmp <- out
                out <- vector(mode = "list", length = nrow(submodels) + 1)
                out[[1]] <- tmp

                for(j in seq(along = submodels$trials))
                  out[[j+1]] <- as.character(predict(modelFit, newdata, trial = submodels$trials[j]))
              }
              out              
            },
            prob = NULL,
            predictors = function(x, ...) {
              vars <- C5imp(x, metric = "splits")
              rownames(vars)[vars$Overall > 0]
            },
            levels = function(x) x$obsLevels,
            varImp = function(object, ...) C5imp(object, ...),
            tags = c("Tree-Based Model", "Rule-Based Model", "Implicit Feature Selection",
                     "Boosting", "Ensemble Model", "Cost Sensitive Learning", "Two Class Only", 
                     "Handle Missing Predictor Data", "Accepts Case Weights"),
            sort = function(x){
              x$model <- factor(as.character(x$model), levels = c("rules", "tree"))
              x[order(x$trials, x$model, !x$winnow, x$costFP,x$costFN),]
            },
            trim = function(x) {
              x$boostResults <- NULL
              x$size <- NULL
              x$call <- NULL
              x$output <- NULL
              x
            })

library(mlbench)
data(Sonar)

library(caret)


set.seed(990)
inTraining <- createDataPartition(Sonar$Class, p = .5, list = FALSE)
inTraining
training <- Sonar[inTraining,]
test <- Sonar[-inTraining,]



set.seed(990)
fitControl <- trainControl(method="repeatedcv", number=10, repeats=5)


statGrid <-  expand.grid(trials = 3,
                         model = "tree",
                         winnow = FALSE,
                         cost = 2)

set.seed(825)


statFit <- train(Class~., data=training, method="C5.0Cost", trControl=fitControl, tuneGrid = statGrid, metric = "Accuracy")


## Example modified to include costs for both false positives and negatives
set.seed(825)
statGridMod <-  expand.grid(trials = 3,
                            model = "tree",
                            winnow = FALSE,
                            costFP = c(1,2,3), #new cost parameters
                            costFN = c(3,2,1)) #new cost parameters

statFit <- train(Class~., data=training, method=modelInfo, trControl=fitControl, tuneGrid = statGridMod, metric = "Accuracy")

statFit
