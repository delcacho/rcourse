# 
# uncomment this line first to install package GGally
library(GGally)
wages <- read.table("http://global.oup.com/booksites/content/0199268010/datasets/ch3/xm301bwa.asc", header= TRUE)
str(wages)

# A simple plot of the data
plot(wages)

# histogram of some columns
par(mfrow = c(1,2))

temp.gender <- c("female", "male")[1+wages$GENDER] # recode the levels as strings
temp.minority <- c("no", "yes")[1+wages$MINORITY]
temp.jobcat <- c("admin", "custodial", "management")[wages$JOBCAT]

wages$GENDER <- as.factor(temp.gender)
wages$MINORITY <- as.factor(temp.minority)
wages$JOBCAT <- as.factor(temp.jobcat)

hist(wages$SALARY)
hist(wages$GENDER)
boxplot(wages$SALARY ~ wages$GENDER, col = c("red", "blue")) # specific array of colours

# this looks much better
ggally_box(
  wages,
  mapping        = ggplot2::aes_string(y = "SALARY", x = "GENDER", color = "GENDER"),
  outlier.colour = "red",
  outlier.shape  = 13,
  outlier.size   = 8
)

names.noID <- !(colnames(wages) %in% c("IDNUMBER")) # names.noID is a boolean vector that can be used for indexing another vector or data.frame
wages <- wages[names.noID] # ignore ID number

primitive.wages <- wages[c("SALARY","EDUC", "SALBEGIN", "GENDER", "MINORITY", "JOBCAT")] # string vectors can also be used for selecting column names
table(primitive.wages[["GENDER"]], primitive.wages[["JOBCAT"]], dnn = c("gender", "jobcat"))
str(primitive.wages)

# average salary by gender
tapply(primitive.wages[["SALARY"]], list(gender = primitive.wages[["GENDER"]]), FUN = mean)

# average salary by gender AND minority AND jobcat: function tapply() to apply any function by groups
tapply(primitive.wages[["SALARY"]], list(gender = primitive.wages[["GENDER"]], 
                                         minority = primitive.wages[["MINORITY"]], 
                                         jobcat = primitive.wages[["JOBCAT"]]), FUN = mean)


# Inspect pairwise correlations
ggpairs(primitive.wages[,c("EDUC", "SALBEGIN", "GENDER", "MINORITY", "JOBCAT", "SALARY")], switch = "both")
dev.new() # open a new window and set it active
dev.set(which = 4)
# now plot everything separated by GENDER
ggpairs(primitive.wages[,c("EDUC", "SALBEGIN", "GENDER", "MINORITY", "JOBCAT", "SALARY")], switch = "both", mapping = ggplot2::aes(colour = GENDER, alpha = 0.4))

model1 <- lm(SALARY ~ . , data = primitive.wages)

model1$contrasts # lets inspect the contrasts

# specify the contrasts as a tagged list, where the contrast is the name of a contrast function object
model1contrsum <- update(model1, . ~ . , contrasts = list(GENDER = contr.sum, MINORITY = contr.sum, JOBCAT = contr.sum))

# inspect the new contrasts we have just set
model1contrsum$contrasts

par(mfrow = c(2,2)) # divide the screen in 2 x 2 cells

model2 <- lm(SALARY ~ EDUC + log(SALBEGIN) + GENDER + MINORITY + JOBCAT, data = primitive.wages)  # log(SALBEGIN)
model3 <- update(model2, log(.) ~ .)  # log(SALARY)
model3b <- update(model3, data = primitive.wages[-c(18, 218, 274),]) # just changed the data: remove observations 18, 218, 274

dev.set(which = 3) # set the inner RStudio window as the active window
par(mfrow = c(2,2))
plot(model3b)

par(mfrow = c(1,1)) # return to single-image screen 
hist(model3b$residuals) # hmmmm ... looks good !
shapiro.test(model3b$residuals) # almost good enough (cannot reject for alpha = 0.01, rejected for alpha = 0.05)

# let's do some predictions
all.predictions = predict(model3b, primitive.wages[1,])
residuals = all.predictions - primitive.wages$SALARY
head(residuals)
