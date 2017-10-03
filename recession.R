rm(list = ls(all = TRUE)) #CLEAR WORKSPACE
#library(doMC)
#registerDoMC(cores = 4)
library(quantmod)
library(zoo)
library(DMwR)
library(caret)
library(kernlab)
library(scales)
library(TTR)
library(zoo)
library(ggplot2)
library(reshape2)

monthMult <- 21
monthMult <- 4

#################################################
# 1. Get Data
#################################################

#Credit Spreads
getSymbols('CP3M',src='FRED') #3-Month Commercial Paper (Old Series)
getSymbols('CPF3M',src='FRED') #3-Month Financial Commercial Paper (New Series)
getSymbols('DBAA',src="FRED")
getSymbols('DAUTOSAAR',src="FRED")
getSymbols('TOTCI',src="FRED")
getSymbols('INVCMRMTSPL',src="FRED")
getSymbols('PERMIT',src="FRED")
getSymbols('PAYEMS',src="FRED")
getSymbols('UNRATE',src="FRED")
getSymbols('DFF',src="FRED")
getSymbols('FEDFUNDS',src="FRED")
getSymbols('IPDCONGD',src="FRED")
getSymbols('UMCSENT1',src="FRED")
getSymbols('UMCSENT',src="FRED")
getSymbols('IC4WSA',src="FRED")
getSymbols('INDPRO',src="FRED")
getSymbols('WILL5000INDFC',src="FRED")
getSymbols('GDP',src="FRED")
getSymbols('WTISPLC',src="FRED")
getSymbols('WPUSI019011',src="FRED")
getSymbols('CFNAI',src="FRED")
getSymbols('CANDH',src="FRED")

ComPaper <- c(CP3M,CPF3M)
ConSent <- c(UMCSENT1,UMCSENT)
getSymbols('DTB3',src='FRED') #3-Month Treasury
CS <- na.omit(ComPaper-DTB3)
names(CS) <- 'CS'
#FEDFUNDS <- DFF
names(FEDFUNDS) <- 'FEDFUNDS'
names(ConSent) <- 'UMCSENT'
#Yield Curve
getSymbols('DGS10',src='FRED')
BAA10YM <- DBAA - DGS10
names(BAA10YM) <- 'BAA10YM'
YC <- na.omit(DGS10-DTB3)
names(YC) <- 'YC'
names(WTISPLC) <- 'OIL'
names(WPUSI019011) <- 'COPPER'

#Kansas City Fed Financial Stress Indicator
getSymbols('KCFSI',src='FRED')
names(KCFSI) <- 'KCFSI'

getSymbols('DAUTOSAAR',src='FRED')
names(DAUTOSAAR) <- 'DAUTOSAAR'
names(INVCMRMTSPL) <- 'INVCMRMTSPL'

#Recessions
getSymbols('USREC',src='FRED') 

#Recessions next 12 months
Target <- Next(USREC,12)
names(Target) <- 'Target'

#Combine into one dataset
Data <- merge.xts(YC,USREC)
Data <- merge.xts(Data,CS)
#Data <- merge.xts(Data,BAA10YM)
Data <- merge.xts(Data,DAUTOSAAR)
Data <- merge.xts(Data,INVCMRMTSPL)
Data <- merge.xts(Data,PERMIT)
Data <- merge.xts(Data,UNRATE)
Data <- merge.xts(Data,TOTCI)
Data <- merge.xts(Data,IPDCONGD)
Data <- merge.xts(Data,PAYEMS)
Data <- merge.xts(Data,FEDFUNDS)
Data <- merge.xts(Data,ConSent)
Data <- merge.xts(Data,INDPRO)
#Data <- merge.xts(Data,WILL5000INDFC)
Data <- merge.xts(Data,GDP)
Data <- merge.xts(Data,WTISPLC)
Data <- merge.xts(Data,WPUSI019011)
Data <- merge.xts(Data,CFNAI)
Data <- merge.xts(Data,CANDH)
Data <- merge.xts(Data,IC4WSA)
#Data$YC2 <- (Data$YC-Lag(Data$YC,6))/Lag(Data$YC,6)
#Data$CS2 <- (Data$CS-Lag(Data$CS,6))/Lag(Data$CS,6)
#Data$MC2GDP <- Data$WILL5000INDFC/Data$GDP
Data <- merge.xts(Data,Target)	#Starts 1921
#Data <- merge.xts(Data,KCFSI)	#Starts 1990
Data <- na.locf(Data)
Data <- Data[xts:::startof(Data, "weeks")]
#################################################
# Build a model
#################################################
#Choose Differencing window
#Hussman uses 6 months
Diff <- 3		
Data$PAYEMS <- (Data$PAYEMS-Lag(Data$PAYEMS,60*monthMult))/Lag(Data$PAYEMS,60*monthMult)
Data$DAUTOSAAR <- (Data$DAUTOSAAR-Lag(Data$DAUTOSAAR,6*monthMult))/Lag(Data$DAUTOSAAR,6*monthMult)
Data$INVCMRMTSPL <- (Data$INVCMRMTSPL-Lag(Data$INVCMRMTSPL,60*monthMult))/Lag(Data$INVCMRMTSPL,60*monthMult)
#Data$PERMIT <- (Data$PERMIT-Lag(Data$PERMIT,18*monthMult))/Lag(Data$PERMIT,18*monthMult)
#Data$IC4WSA <- (Data$IC4WSA-Lag(Data$IC4WSA,6*monthMult))/Lag(Data$IC4WSA,6*monthMult)
Data$IPDCONGD <- (Data$IPDCONGD-Lag(Data$IPDCONGD,6*monthMult))/Lag(Data$IPDCONGD,6*monthMult)
Data$FEDFUNDS <- sign(Data$FEDFUNDS-Lag(Data$FEDFUNDS,24*monthMult))
Data$UMCSENT <- (Data$UMCSENT-Lag(Data$UMCSENT,60*monthMult))/Lag(Data$UMCSENT,60*monthMult)
Data$INDPRO <- (Data$INDPRO-Lag(Data$INDPRO,60*monthMult))/Lag(Data$INDPRO,60*monthMult)
#Data$MC2GDP <- (Data$MC2GDP-Lag(Data$MC2GDP,4*monthMult))/Lag(Data$MC2GDP,4*monthMult)
#Data$OIL <- (Data$OIL-Lag(Data$OIL,36*monthMult))/Lag(Data$OIL,36*monthMult)
#Data$COPPER <- (Data$COPPER-Lag(Data$COPPER,24*monthMult))/Lag(Data$COPPER,24*monthMult)
Data$CANDH <- SMA(Data$CANDH,5*monthMult)-Lag(SMA(Data$CANDH,5*monthMult),24*monthMult)
Data$CFNAI <- SMA(Data$CFNAI,5*monthMult)-Lag(SMA(Data$CFNAI,5*monthMult),36*monthMult)
Data$IC4WSA <- SMA(Data$IC4WSA,5*monthMult)-Lag(SMA(Data$IC4WSA,5*monthMult),6*monthMult)
Data$PERMIT <- SMA(Data$PERMIT,5*monthMult)-Lag(SMA(Data$PERMIT,5*monthMult),18*monthMult)
Data$TOTCI <- (SMA(Data$TOTCI,12*monthMult)-Lag(SMA(Data$TOTCI,12*monthMult),2*monthMult))/Lag(SMA(Data$TOTCI,12*monthMult),2*monthMult)
#Data$BUSLOANS <- Data$BUSLOANS-Lag(Data$BUSLOANS,36*monthMult)/Lag(Data$BUSLOANS,36*monthMult)
Data$OIL <- (SMA(Data$OIL,5*monthMult)-Lag(SMA(Data$OIL,5*monthMult),36*monthMult))
Data$COPPER <- (SMA(Data$COPPER,5*monthMult)-Lag(SMA(Data$COPPER,5*monthMult),24*monthMult))/Lag(SMA(Data$COPPER,5*monthMult),24*monthMult)
Data$WILL5000INDFC <- NULL
Data$GDP <- NULL

Data$BUSLOANS <- NULL
#Data$TOTCI <- NULL
#Data$CFNAI <- NULL
#Data$MC2GDP <- NULL
#Data$UMCSENT <- NULL
#Data$BAA10YM <- NULL
#Data$FEDFUNDS <- NULL
Data$MC2GDP <- NULL
#Data$USREC <- NULL
#Data$DAUTOSAAR <- NULL
#Omit rows missing X vars
Keep <- apply(is.na(Data[,-1]),1,sum)==0
FullData <- Data
Data <- Data[Keep&complete.cases(Data$Target),]
last.date <-tail(index(Data$YC),1)
period <- 510

#Examine dataset
library(caTools)
head(Data)
sum(Data$USREC)
USREC.A <-tail(as.numeric(window(Data$USREC, end=(last.date))),period)
USREC.a <-tail(data.frame(window(Data$USREC, end=(last.date))),period)
AUCdata <- na.omit(Data)
print(colAUC(AUCdata[,-ncol(AUCdata)],AUCdata[,ncol(AUCdata)],plot=TRUE))

#Model building function
#Takes an index, returns a prediction for the next index
fitmodel <- function(index,...) {

	#Load Relevant Data
	HistData <- data.frame(Data[1:(index-1),])
        HistData$Target <- as.factor(HistData$Target)
        #HistData <- upSample(x = HistData[, -ncol(HistData)], y = Target)   
        HistData <- SMOTE(Target~., data=HistData)
	CurrentData <- data.frame(Data[index,])
        HistData$Target <- make.names(HistData$Target)

	#Fit model
	model <- train(Target~.,HistData,metric = "ROC", ...,
					trControl=trainControl(method='boot632',
                                                number=100,
						classProbs=TRUE,
						#summaryFunction = twoClassSummary,
						verboseIter=FALSE),trace=FALSE,
					preProc=c("center","scale")
)					
	#Predict for next period
	out <- list(raw=predict(model,CurrentData,"raw")[[1]],prob=predict(model,CurrentData,"prob")[[1]],model=model)
	row.names(out) <- NULL
	return(out)
}

#Use at least 12 months of recessions for training
TotalRec <- na.omit(as.numeric(cumsum(Data$Target)))
Start <- (1:length(TotalRec))[TotalRec==12]

#Roll the model through the dataset
require(pbapply)
indexes <- seq(Start,nrow(Data))		
result <- pbsapply(indexes,fitmodel,method='glmStepAIC',tuneLength=1)
model <- result["model",ncol(result)]
print(predict(model,FullData,"raw"))
print(tail(FullData))
result <- result[c("raw","prob"),]
result["raw",] <- as.character(unlist(result["raw",]))
result2 <- as.data.frame(apply(result,1,function(x) unlist(x)))
predictions <- result2$raw
probabilities <- result2$prob

Data$pTarget <- c(rep(NA,Start-1),as.numeric(predictions))

#################################################
# Evaluate model
#################################################
pTarget <- predictions
Target <- make.names(as.factor(Data$Target))
Target <- Target[(length(Target)-length(pTarget)+1):length(Target)]

#Confusion matrix
print(confusionMatrix(pTarget,Target,positive = 'X1'))

treas.sp.p.f <-1-as.numeric(as.character(probabilities))
treas.sp.pct <- treas.sp.p.f#pnorm(treas.sp.p.f)

# Generate data for chart
# Treasury yield curve

date <-as.xts(index(tail((window(Data$YC, end=(last.date))),period)))
treas.probit.1 <-merge(date,treas.sp.pct)
date.a <-as.xts(index(tail((window(Data$USREC, end=(last.date))),period)))
usrec.1 <-merge(date.a,USREC.A)
probit.dates <-tail(treas.probit.1,period)
dates.1 <- index(probit.dates)

n.1 <-length(dates.1)
z.1 <-tail(dates.1,1)
b.1 = data.frame(dates.1, treas.probit.1)
c.1 = melt(b.1, id.vars = "dates.1")

# US Recession Signal
probit.dates.rec <-tail(usrec.1,period)
dates.1.rec <- index(probit.dates.rec)

n.1.rec <-length(dates.1.rec)
z.1.rec <-tail(dates.1.rec,1)
b.1.rec = data.frame(dates.1.rec, usrec.1)

c.1.rec = melt(b.1.rec, id.vars = "dates.1.rec")

# Generate chart
p1 <- ggplot(c.1.rec, aes(x = dates.1.rec, y = value)) + theme_bw(12)+
geom_bar(stat = "identity",colour="gray",alpha=0.1) +
ggtitle("Probit Estimated Recession Probabilities") +

theme(legend.position="none") +
theme(axis.title.x = element_blank()) +
theme(axis.title.y = element_blank())

p1 <-p1+geom_line(data=c.1, aes(x=dates.1, y=value), colour="blue")+scale_x_date(date_breaks="5 years",date_minor_breaks="1 year",date_labels="%Y")
p1 # End1
