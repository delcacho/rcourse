source("cumulative.R")
source("qini.R")

url = "http://www.minethatdata.com/Kevin_Hillstrom_MineThatData_E-MailAnalytics_DataMiningChallenge_2008.03.20.csv"
hc = read.csv(file=url)

hc$treat <- ifelse(as.character(hc$segment) != "No E-Mail", TRUE, FALSE)
hc$mens <- as.factor(hc$mens)
hc$womens <- as.factor(hc$womens)
hc$newbie <- as.factor(hc$newbie)

require(uplift)
require(plyr)
count(hc, "segment")

hc_rvtu <- rvtu(visit~recency+history_segment+history+mens+womens+zip_code+newbie+channel+trt(as.numeric(treat)),
                data=hc[hc$segment != "Mens E-Mail",],
                method="none")
names(hc_rvtu)

explore(y~recency+history_segment+history+mens+womens+zip_code+newbie+channel+trt(ct),
        data=hc_rvtu)
# targeted
count(hc_rvtu[hc_rvtu$ct == 1,], "y")$freq / sum(hc_rvtu$ct == 1)
# control
count(hc_rvtu[hc_rvtu$ct == 0,], "y")$freq / sum(hc_rvtu$ct == 0)

par(mfrow=c(2,3))
boxplot(recency~visit, data=hc[hc$treat,],
        ylab="recency", xlab="visit")
boxplot(recency~conversion, data=hc[hc$treat,],
        ylab="recency", xlab="conversion")
boxplot(split(hc$recency[hc$spend != 0 & hc$treat],
              cut(hc$spend[hc$spend != 0 & hc$treat], 3)),
        ylab="recency", xlab="spend for converted")
mtext("recency target", side=3, line=-3, outer=TRUE, cex=2, font=2)
boxplot(recency~visit, data=hc[! hc$treat,],
        ylab="recency", xlab="visit")
boxplot(recency~conversion, data=hc[hc$treat,],
        ylab="recency", xlab="conversion")
boxplot(split(hc$recency[hc$spend != 0 & ! hc$treat],
              cut(hc$spend[hc$spend != 0 & ! hc$treat], 3)),
        ylab="recency", xlab="spend for converted")
mtext("recency control", side=3, line=-39, outer=TRUE, cex=2, font=2)

pairs(~recency+history_segment+history,
      data=hc_rvtu, col=hc_rvtu$y+1)

logit.formula <- ~recency+mens+womens+zip_code+newbie+channel

logit.formula.interactions <- as.formula(paste("~history_segment*mens+history_segment*womens",
                                               "history_segment*newbie",
                                               "zip_code*mens+zip_code*womens+zip_code*newbie",
                                               "channel*mens+channel*womens+channel*newbie",
                                               "channel*history_segment",
                                               sep="+"))

set.seed(1024)
require(glmnet)
logit.x.interactions <- model.matrix(logit.formula.interactions, data=hc_rvtu)
logit.z <- hc_rvtu$z
logit.y <- hc_rvtu$y

# traditional classifier, y as response
logit.cv.lasso.y.interactions <- cv.glmnet(logit.x.interactions, logit.y, alpha=1, family="binomial")
plot(logit.cv.lasso.y.interactions)

# uplift classifier, z as response
logit.cv.lasso.z.interactions <- cv.glmnet(logit.x.interactions, logit.z, alpha=1, family="binomial")
plot(logit.cv.lasso.z.interactions)

coef(logit.cv.lasso.z.interactions)[which(coef(logit.cv.lasso.z.interactions) != 0),]

coef(logit.cv.lasso.z.interactions,
     s=logit.cv.lasso.z.interactions$lambda.min)[which(coef(logit.cv.lasso.z.interactions,
                                                            s=logit.cv.lasso.z.interactions$lambda.min) != 0),]

require(ROCR)
preds.y.interactions.1se <- predict(logit.cv.lasso.y.interactions, logit.x.interactions,
                                    s=logit.cv.lasso.y.interactions$lambda.1se, type="response")
pred.y <- prediction(preds.y.interactions.1se, hc_rvtu$y)
auc.y <- ROCR:::performance(pred.y, "auc")
as.numeric(auc.y@y.values)

preds.z.interactions.1se <- predict(logit.cv.lasso.z.interactions, logit.x.interactions,
                                    s=logit.cv.lasso.z.interactions$lambda.1se, type="response")
pred.z <- prediction(preds.z.interactions.1se, hc_rvtu$z)
auc.z <- ROCR:::performance(pred.z, "auc")
as.numeric(auc.z@y.values)

qini_from_data(preds.z.interactions.1se,
               hc_rvtu,
               plotit=TRUE,
               x_axis_ratio=TRUE)

# 1. compare cumulative responses just taking into account classifications (not much sense for the uplift)
par(mfrow=c(2,1))
cumulative_response(preds.y.interactions.1se,
                    hc_rvtu,
                    "y",
                    x_axis_ratio=TRUE)

cumulative_response(preds.z.interactions.1se,
                    hc_rvtu,
                    plotit=TRUE,
                    x_axis_ratio=TRUE)


# 2. compare cumulative penalized as in qini (of course traditional classifier is worse)
par(mfrow=c(2,1))
cum_penalized_curve(preds.y.interactions.1se,
                    hc_rvtu,
                    "y",
                    "z",
                    x_axis_ratio=TRUE)

cum_penalized_curve(preds.z.interactions.1se,
                    hc_rvtu,
                    "y",
                    "ct",
                    x_axis_ratio=TRUE)
