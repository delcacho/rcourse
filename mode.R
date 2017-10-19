#exmple 7 credit 
credit <- read.csv("credit.csv")
head(credit)
library(stringr)
colnames(credit)
table(complete.cases(credit)) #no NAs but there are unknowns that have to be considered NA
credit$checking_balance[credit$checking_balance=="unknown"] <- NA #so assign na to unknown 
# now remove unknown level by replacing it 
credit$checking_balance <- factor(credit$checking_balance) #so you dont have unknown but have NA 
summary(credit)
#use apply function to see in which columns we find the word unknown (and how many times)
apply(credit,2,function(x) sum(str_detect(x,".*unknown")))
#remove unknown from saving balance and delete level 
credit$savings_balance[credit$savings_balance=="unknown"] <- NA
credit$savings_balance <- factor(credit$savings_balance)
credit$property[credit$property=="unknown/none"] <- NA
credit$property <- factor(credit$property)
summary(credit)
#now we have to replace NAs (factor - mode, numerical - median) the 3 we have to replace are factors so replace by mode
#need to calculate the mode
mode <- function(x){
  names(which.max(table(x)))[1]
}
countNA <- apply(credit,2,function(x) sum(is.na(x)))
unknownIndices <- apply(credit,2,function(x) is.na(x))
for (col in names(credit)[countNA>0]){
  credit[unknownIndices,col] = mode(credit[!unknownIndices[,col],col])
}

