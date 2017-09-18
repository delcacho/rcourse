normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x))) }

prc <- read.csv("Prostate_Cancer.csv",stringsAsFactors = FALSE)    #This command imports the required data set and saves it to the prc data frame.

str(prc) 

prc <- prc[-1]  #removes the first variable(id) from the data set.

table(prc$diagnosis_result)  # it helps us to get the numbers of patients

prc$diagnosis <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))

round(prop.table(table(prc$diagnosis)) * 100, digits = 1)  # it gives the result in the percentage form rounded of to 1 decimal place( and so it’s digits = 1)

prc_n <- as.data.frame(lapply(prc[2:9], normalize))

summary(prc_n$radius)

prc_train <- prc_n[1:65,]
prc_test <- prc_n[66:100,]

prc_train_labels <- prc[1:65, 1]
prc_test_labels <- prc[66:100, 1]   #This code takes the diagnosis factor in column 1 of the prc data frame and on turn creates prc_train_labels and prc_test_labels data frame.

library(class)

prc_test_pred <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=10)

library(gmodels)

CrossTable(x = prc_test_labels, y = prc_test_pred, prop.chisq=FALSE)
