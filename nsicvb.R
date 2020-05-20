#Read in Data
vb.df <- read.csv("vbdata.csv")

#Create factors/ as numerics
vb.df$WinLoss <- as.factor(vb.df$WinLoss)
vb.df$HomeAway <- as.factor(vb.df$HomeAway)
vb.df$SP <- as.numeric(as.character(vb.df$SP))
vb.df$Kills <- as.numeric(as.character(vb.df$Kills))
vb.df$Errors <- as.numeric(as.character(vb.df$Errors))
vb.df$AttackAttempts <- as.numeric(as.character(vb.df$AttackAttempts))
vb.df$Assists <- as.numeric(as.character(vb.df$Assists))
vb.df$HittingPercentage <- as.numeric(as.character(vb.df$HittingPercentage))
vb.df$ServiceAces <- as.numeric(as.character(vb.df$ServiceAces))
vb.df$ServiceErrors <- as.numeric(as.character(vb.df$ServiceErrors))
vb.df$ReceptionErrors <- as.numeric(as.character(vb.df$ReceptionErrors))
vb.df$Digs <- as.numeric(as.character(vb.df$Digs))
vb.df$BlockSolo <- as.numeric(as.character(vb.df$BlockSolo))
vb.df$BlockAssists <- as.numeric(as.character(vb.df$BlockAssists))
vb.df$BlockingErrors <- as.numeric(as.character(vb.df$BlockingErrors))
vb.df$TotalBlocks <- as.numeric(as.character(vb.df$TotalBlocks))
vb.df$BHE <- as.numeric(as.character(vb.df$BHE))

#Get data in terms of sets played
vb.df$KillsSP <- vb.df$Kills / vb.df$SP
vb.df$AESP <- vb.df$Errors / vb.df$SP
vb.df$AttackAttemptsSP <- vb.df$AttackAttempts / vb.df$SP
vb.df$AssistsSP <- vb.df$Assists / vb.df$SP
vb.df$SASP <- vb.df$ServiceAces / vb.df$SP
vb.df$SESP <- vb.df$ServiceErrors / vb.df$SP
vb.df$RESP <- vb.df$ReceptionErrors / vb.df$SP
vb.df$DigsSP <- vb.df$Digs / vb.df$SP
vb.df$BSSP <- vb.df$BlockSolo / vb.df$SP
vb.df$BASP <- vb.df$BlockAssists / vb.df$SP
vb.df$BESP <- vb.df$BlockingErrors / vb.df$SP
vb.df$TBSP <- vb.df$TotalBlocks / vb.df$SP
vb.df$BHESP <- vb.df$BHE / vb.df$SP

#Remove unneeded columns
vb.df <- vb.df[, -c(1:2)]
vb.df <- vb.df[, -c(3:4)]
vb.df <- vb.df[, -c(4:6)]   
vb.df <- vb.df[, -c(5:15)] 
vb.df <- vb.df[, -c(5:8)]

#Format Decimals
vb.df$KillsSP <- format(round(vb.df$KillsSP, 2), nmsall = 2)
vb.df$AESP <- format(round(vb.df$AESP, 2), nmsall = 2)
vb.df$AttackAttemptsSP <- format(round(vb.df$AttackAttemptsSP, 2), nmsall = 2)
vb.df$AssistsSP <- format(round(vb.df$AssistsSP, 2), nmsall = 2)
vb.df$SASP <- format(round(vb.df$SASP, 2), nmsall = 2)
vb.df$SESP <- format(round(vb.df$SESP, 2), nmsall = 2)
vb.df$RESP <- format(round(vb.df$RESP, 2), nmsall = 2)
vb.df$DigsSP <- format(round(vb.df$DigsSP, 2), nmsall = 2)
vb.df$BSSP <- format(round(vb.df$BSSP, 2), nmsall = 2)
vb.df$BASP <- format(round(vb.df$BASP, 2), nmsall = 2)
vb.df$BESP <- format(round(vb.df$BESP, 2), nmsall = 2)
vb.df$TBSP <- format(round(vb.df$TBSP, 2), nmsall = 2)
vb.df$BHESP <- format(round(vb.df$BHESP, 2), nmsall = 2)

#Make numeric again
vb.df$KillsSP <- as.numeric(as.character(vb.df$KillsSP))
vb.df$AESP <- as.numeric(as.character(vb.df$AESP))
vb.df$AttackAttemptsSP <- as.numeric(as.character(vb.df$AttackAttemptsSP))
vb.df$AssistsSP <- as.numeric(as.character(vb.df$AssistsSP))
vb.df$SASP <- as.numeric(as.character(vb.df$SASP))
vb.df$SESP <- as.numeric(as.character(vb.df$SESP))
vb.df$RESP <- as.numeric(as.character(vb.df$RESP))
vb.df$DigsSP <- as.numeric(as.character(vb.df$DigsSP))
vb.df$BSSP <- as.numeric(as.character(vb.df$BSSP))
vb.df$BASP <- as.numeric(as.character(vb.df$BASP))
vb.df$BESP <- as.numeric(as.character(vb.df$BESP))
vb.df$TBSP <- as.numeric(as.character(vb.df$TBSP))
vb.df$BHESP <- as.numeric(as.character(vb.df$BHESP))



#Tree Regression
library(rpart)
library(rpart.plot)

set.seed(1)
train.index <- sample(c(1:dim(vb.df)[1]), dim(vb.df)[1]*.6)
train.df <- vb.df[train.index,]
valid.df <- vb.df[-train.index,]

default.ct <- rpart(WinLoss ~ HittingPercentage + SASP + SESP + DigsSP + TBSP + HomeAway, data = train.df, method = "class", minbucket = 50, maxdepth = 7)
prp(default.ct, type= 1, extra = 1, under = TRUE, split.font= 1, varlen = -10,
    box.col=ifelse(default.ct$frame$var == "<leaf>", 'gray', 'white'))
default.ct <- rpart(WinLoss ~ SESP, data = train.df, method = "class", minbucket = 50, maxdepth = 7)
prp(default.ct, type= 1, extra = 1, under = TRUE, split.font= 1, varlen = -10,
    box.col=ifelse(default.ct$frame$var == "<leaf>", 'gray', 'white'))

#Tree that gives most leaves
default.ct <- rpart(WinLoss ~ ., data = train.df, method = "class", cp=.01)
prp(default.ct, type= 1, extra = 1, under = TRUE, split.font= 1, varlen = -10,
    box.col=ifelse(default.ct$frame$var == "<leaf>", 'gray', 'white'))
printcp(default.ct)
pruned.ct <- prune(default.ct, cp = .1)
pruned.ct <- prune(default.ct, cp = default.ct$cptable[which.min(default.ct$cptable[, "xerror"]), "CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, under= TRUE, split.font =1, varlen = -10)
#Build Confusion Matrix
library(caret)
ct.pred.train <- predict(pruned.ct, train.df, type = "class")
confusionMatrix(ct.pred.train, train.df$WinLoss)
ct.pred.valid <- predict(pruned.ct, valid.df, type = "class")
confusionMatrix(ct.pred.valid, valid.df$WinLoss)

ct.pred.valid <- predict(default.ct, valid.df, type = "class")
confusionMatrix(ct.pred.valid, valid.df$WinLoss)

#Try random Forest
library(randomForest)
rf <- randomForest(as.factor(WinLoss)~ . , data = train.df, ntree = 500, mtry = 4,
                   nodesize = 7, importance = TRUE)
#IMPORTANCE PLOTTT
varImpPlot(rf, type = 1)
#Confusion Matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$WinLoss)

default.ct <- rpart(WinLoss ~ ., data = train.df, method = "class", cp=.05)
prp(default.ct, type= 1, extra = 1, under = TRUE, split.font= 1, varlen = -10,
    box.col=ifelse(default.ct$frame$var == "<leaf>", 'gray', 'white'))
#Build Confusion Matrix
library(caret)
ct.pred.train <- predict(default.ct, train.df, type = "class")
confusionMatrix(ct.pred.train, train.df$WinLoss)
ct.pred.valid <- predict(default.ct, valid.df, type = "class")
confusionMatrix(ct.pred.valid, valid.df$WinLoss)

#Boosted Tree
library(adabag)
library(rpart)
library(caret)
boost <- boosting(WinLoss ~ ., data = train.df)
pred <- predict(boost, valid.df)
confusionMatrix(pred$class, valid.df$WinLoss)

#Linear Regression
lm <- lm(as.numeric(WinLoss) ~ ., data = train.df)
summary(lm)
#Logistic Regression
logit.reg <- glm(WinLoss ~ ., data = train.df, family = "binomial")
summary(logit.reg)

#Visualizations
library(ggplot)
ggplot(vb.df)+ geom_point(aes(x=WinLoss, y=HittingPercentage), colour="navy", alpha=0.7)
ggplot(vb.df)+ geom_boxplot(aes(x=as.factor(WinLoss), y=HittingPercentage)) + xlab("WinLoss")

ggplot(vb.df)+ geom_boxplot(aes(x=as.factor(WinLoss), y=DigsSP)) + xlab("WinLoss")
ggplot(vb.df)+ geom_boxplot(aes(x=as.factor(WinLoss), y=AssistsSP)) + xlab("WinLoss")
ggplot(vb.df)+ geom_boxplot(aes(x=as.factor(WinLoss), y=KillsSP)) + xlab("WinLoss")
par(mfcol = c(1,4))
boxplot(vb.df$HittingPercentage ~ 
          vb.df$WinLoss, xlab="WinLoss",
        ylab="Hitting Percentage")
boxplot(vb.df$KillsSP ~ 
          vb.df$WinLoss, xlab="WinLoss", 
        ylab="Kills/Set")
boxplot(vb.df$SASP ~ 
          vb.df$WinLoss, xlab="WinLoss", 
        ylab="Aces/Set")
boxplot(vb.df$DigsSP ~ 
          vb.df$WinLoss, xlab="WinLoss",
        ylab="Digs/Set")

#HeatMap/Correlation Table
library(gplots)
vb2.df <- vb.df[, -c(1:2)]
heatmap(cor(vb.df), Rowv = NA, Colv = NA)
heatmap.2(cor(vb2.df), Rowv = NA, Colv = NA, dendrogram = "none", 
          cellnote = round(cor(vb2.df), 2), notecol = "black", key = FALSE,
          trace = 'none', margins = c(10, 10))
        
library(GGally)
plot(vb.df)
ggpairs(vb.df)
ggpairs(vb.df[, c(1, 2)])
ggpairs(vb.df[, c(4, 5, 6, 8, 9, 12, 14, 16)])
plot(vb.df[, c(5, 6, 7, 10, 11, 13, 15)])

#General variable info
summary(vb.df)
vbquant.df <- vb.df[, -c(2)]
vbquant.df$WinLoss <- as.numeric(vb.df$WinLoss)
data.frame(mean= sapply(vbquant.df, mean), sd=sapply(vbquant.df, sd),
           min= sapply(vbquant.df, min), max= sapply(vbquant.df, max),
           median= sapply(vbquant.df, median), length = sapply(vbquant.df, length),
           miss.val= sapply(vbquant.df, function(x) sum(length(which(is.na(x))))))

#Begin PCS
vb.df$WinLoss <- as.factor(vb.df$WinLoss)
pcs <- prcomp(vb.df)
summary(pcs)
pcs$rot
scores <- pcs$x
head(scores, 5)
#PCS with normalized
pcs.cor <- prcomp(vbquant.df, scale. = T)
summary(pcs.cor)
pcs.cor$rot
scores <- pcs.cor$x
head(scores, 5)




