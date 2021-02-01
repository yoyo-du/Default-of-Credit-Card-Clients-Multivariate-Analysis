library(readxl)
library(ggplot2)
library(GGally)
library(magrittr)
library(tidyverse)
library(scales)
library(ggiraph)
library(ggiraphExtra)
library(plotly)
library(treemapify)
library(moonBook)
library(corrplot)
library(vcd)
library(htmlwidgets)
library(reshape2)
library(scales)
library(dplyr)
library(tree)
library(randomForest)
library(FactoMineR)
library(factoextra)
library(DMwR)
library(caret)
library(pROC)
library(MLmetrics)
library(lift)
library(kernlab)
library(gbm)
library(rpart)
library(ROCR)
#library(Factoshiny)

credit <- read_excel("/Users/yuedu/STA541/project/default.xls")
str(credit)
table(is.na(credit))

attach(credit)

#data processing 
credit$EDUCATION[credit$EDUCATION==0] <- 4
credit$EDUCATION[credit$EDUCATION==5] <- 4
credit$EDUCATION[credit$EDUCATION==6] <- 4

credit$MARRIAGE[credit$MARRIAGE==0] <- 3

credit$PAY_1[credit$PAY_1==-2] <- -1
credit$PAY_2[credit$PAY_2==-2] <- -1
credit$PAY_3[credit$PAY_3==-2] <- -1
credit$PAY_4[credit$PAY_4==-2] <- -1
credit$PAY_5[credit$PAY_5==-2] <- -1
credit$PAY_6[credit$PAY_6==-2] <- -1

credit$PAY_1[credit$PAY_1==0] <- -1
credit$PAY_2[credit$PAY_2==0] <- -1
credit$PAY_3[credit$PAY_3==0] <- -1
credit$PAY_4[credit$PAY_4==-0] <- -1
credit$PAY_5[credit$PAY_5==-0] <- -1
credit$PAY_6[credit$PAY_6==-0] <- -1

#detach(credit)
#attach(credit)

#transfer variables to categorical variables
credit$SEX <- as.factor(credit$SEX)
credit$EDUCATION <- as.factor(credit$EDUCATION)
credit$MARRIAGE <- as.factor(credit$MARRIAGE)

credit$PAY_1 <- as.factor(credit$PAY_1)
credit$PAY_2 <- as.factor(credit$PAY_2)
credit$PAY_3 <- as.factor(credit$PAY_3)
credit$PAY_4 <- as.factor(credit$PAY_4)
credit$PAY_5 <- as.factor(credit$PAY_5)
credit$PAY_6 <- as.factor(credit$PAY_6)
credit$default <- as.factor(credit$default)

sex_default <- table(credit$SEX, credit$default)# indicate dependence
chisq.test(sex_default)
sex_default

#data visualizing numeric
X <- credit[, c(2,6,13:18)]
corr_matrix <- cor(X)
corrplot.mixed(corr_matrix)

corr_matrix <- cor(Y)
corrplot.mixed(corr_matrix)
## check the variance
Y <- credit[, c(2,6,13:24)]
stand_dev <- apply(Y, 2, sd)
stand_df <- data.frame(variable = num_variables, stdev = stand_dev)
ggplot(stand_df, aes(x=variable, y=stdev)) + geom_point() + ggtitle("Plot of Standard Deviations of Numeric Variables")

## visualize categorical variables
mosaic(~default+EDUCATION,credit,direction='v',shade=T,legend=FALSE)
mosaic(~default+MARRIAGE,credit,direction='v',shade=T,legend=FALSE)
mosaic(~MARRIAGE+SEX,credit,direction='v',shade=T,legend=FALSE)
mosaic(~default+EDUCATION+SEX,credit,direction='v',shade=T,legend=FALSE)

## visualize default
dat<- credit%>%dplyr::count(default)%>% mutate(prop=n/sum(n)*100)
dat
g<-ggplot(dat,aes(x = default, tooltip = paste0(round(prop,2),'%'),
                  data_id = default, y=prop/100))+
  geom_bar_interactive(stat='identity',fill = "cornflowerblue", color="black")+
  scale_y_continuous(labels = scales::percent)+
  labs(y='Percent',title='Percent of credit card default')
girafe(print(g))

g<-ggPie(credit,aes(pies=default))
g

g<-ggPie(credit,aes(pies=SEX)) 
g

g<-ggPie(credit,aes(pies=EDUCATION)) 
g

g<-ggPie(credit,aes(pies=MARRIAGE)) 
g

#marriage
dat <- credit%>%dplyr::count(MARRIAGE)%>% mutate(prop=n/sum(n)*100)
ggplot(dat, aes(x=MARRIAGE, y=prop, fill=MARRIAGE))+geom_bar(stat = "identity")

#education
dat <- credit%>%dplyr::count(EDUCATION)%>% mutate(prop=n/sum(n)*100)
ggplot(dat, aes(x=EDUCATION, y=prop, fill=EDUCATION))+geom_bar(stat = "identity")+ylab("percentage(%)")
g<-ggPie(credit,aes(pies=EDUCATION))
g

## visualize limit balance
g<-ggplot(credit, aes(x = LIMIT_BAL)) +
  geom_density(tooltip="Kernal Density", data_id="Kernal Density",color='red')+
  geom_histogram(aes(y= ..density..,tooltip = round(..density..,2),
                                   data_id = LIMIT_BAL),
                             fill = "cornflowerblue",color = "white",bins = 20,alpha=0.5)+xlab("LIMIT_BAL")
girafe(print(g))

#saveWidget(girafe(print(g)), file="/Users/yuedu/STA 541/bar.html")

#visualization 
dat<-credit%>%group_by(Class,Survived)%>%summarise(Freq=sum(Freq))
dat2<- credit%>%pivot_wider(id_cols = PAY_1,names_from = 2,values_from = 3)
dat2

ggplot(credit, aes(fill=PAY_1, )) + 
  geom_bar(position="fill", stat="identity")

dat1 <- credit[, c(1,7:12)]
# transfer to long format

dat2 <- melt(dat1, id.vars = "ID")
dat<-dat2%>%group_by(variable)%>%dplyr::count(value)%>% mutate(prop=n/sum(n)*100)
dat=transform(dat,tooltip=paste0('variable=',variable,', ','value=',value,', ','prop=',prop))

ggBar(dat,aes(x=variable,fill=value,y=prop),stat="identity",position='fill', horizontal=FALSE,width=0.5)+ylab("percent")

#
dat1 <- credit[, c(1,13:18)]
dat<-credit%>%dplyr::group_by(default)%>%summarise(Freq=sum(Freq))
dat2<- credit%>%pivot_wider(id_cols = PAY_1,names_from = 2,values_from = 3)
dat2
#CCA
cname = colnames(credit)
num_variables = cname[c(2,6,13:24)]
person_variables = cname[2:6]
history_variables = cname[7:24]

ggpairs(credit, person_variables, title = "Within Personal Variables",
        lower = list(continuous = wrap("points", alpha = 0.3,
      size = 0.1, color = "cornflowerblue"), combo = 'blank', discrete = 'blank'), upper = list(continuous = 'cor', discrete = 'blank'))

ggpairs(credit, history_variables, title = "Within Historical Variables",
        lower = list(continuous = wrap("points", alpha = 0.3,
                                       size = 0.1, color = "cornflowerblue")))
                                                                                                           

# Tree map of education
plotdata <- credit %>%  dplyr::count(EDUCATION)
ggplot(plotdata, 
       aes(fill = EDUCATION, 
           area = n, 
           label = EDUCATION))+
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "EDUCATION") +
  theme(legend.position = "none")

ggPieDonut(credit,aes(pies=SEX,donuts=default),interactive=TRUE)

# data visualization continuous variable and categorical variables
g<-ggplot(Salaries,aes(x =salary, color = rank,fill=rank,data_id=rank,tooltip=rank))+
  geom_density_interactive(alpha = 0.4) +
  theme(legend.position = "bottom",axis.title.y = element_text(angle=0))
g

g<-p+geom_jitter(alpha = 0.7,
                 size = 1.5) 
g

g<-ggplot(credit, aes(x = AGE, color=default)) +
  geom_point() +  labs(title = "Academic salary by rank and years since degree")
g

#numeric 
p<-ggplot(credit, 
          aes(x = default,y=AGE,color=default))+
  labs(title = "Academic Salary by Rank",
       subtitle = " 9-month salary for 2008-2009",
       x = "",
       y = "")+theme(legend.position = "none")
g<-p+geom_boxplot(aes(fill=default),notch = TRUE,alpha = .3)
g

g<-ggplot(credit,aes(x = LIMIT_BAL, color = default,fill=default,data_id=default,tooltip=default))+
  geom_density_interactive(alpha = 0.4) +
  theme(legend.position = "bottom",axis.title.y = element_text(angle=0))
g

g<-ggplot(credit,aes(x = BILL_AMT6, color = default,fill=default,data_id=default,tooltip=default))+
  geom_density_interactive(alpha = 0.4) +
  theme(legend.position = "bottom",axis.title.y = element_text(angle=0))
g

g<-ggplot(credit,aes(x = PAY_AMT5, color = default,fill=default,data_id=default,tooltip=default))+
  geom_density_interactive(alpha = 0.4) +
  theme(legend.position = "bottom",axis.title.y = element_text(angle=0))
g

## split training and test set
set.seed(2021)
dt = sample(nrow(credit), nrow(credit)*.7)
train <- credit[dt,]
test <- credit[-dt,]

## MFA
res.mfa <- MFA(train[, 2:24], group = c(1, 3, 1, 6, 6, 6), type = c("s", "n", "s", "n", "s", "s"), 
               name.group = c("given credit", "demographic", "age", "history payment status", "bill statement", "previous payment" ),
               graph = FALSE)
print(res.mfa)

# eigenvalues/variances
eig.val <- get_eigenvalue(res.mfa)
head(eig.val)

# screeplot 
fviz_screeplot(res.mfa)

# group of variables
group <- get_mfa_var(res.mfa, "group")
group

group$contrib
group$coord

fviz_mfa_var(res.mfa, "group")
fviz_contrib(res.mfa, "group", axes = 1)
fviz_contrib(res.mfa, "group", axes = 2)

## quantitative variables
quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var
quanti.var$contrib

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE)

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"), legend = "bottom")

# contributions to dimension 1
fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20, palette = "jco")

# contributions to dimension 2
fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20, palette = "jco")

fviz_mfa_var(res.mfa, "quanti.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))

# Color by cos2 values: quality on the factor map
# cos2 values representing the quality of representation on the factor map.
# If a variable is well represented by two dimensions, the sum of the cos2 is closed to one.
fviz_mfa_var(res.mfa, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE)

fviz_cos2(res.mfa, choice = "quanti.var", axes = 1)
fviz_cos2(res.mfa, choice = "quanti.var", axes = 2)

#categorical variables
quali.var <- get_mfa_var(res.mfa, "quali.var")
quali.var
quali.var$contrib

fviz_mfa_var(res.mfa, "quali.var", palette = "jco",
             col.var.sup = "violet", repel = TRUE)

fviz_mfa_var(res.mfa, "quali.var", col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE,
             geom = c("point", "text"))

## graph of individuals 
ind <- get_mfa_ind(res.mfa)
ind$coord

new = data.frame(train[25], ind$coord)
ggplot(new, aes(x = Dim.1, y = Dim.2)) + geom_point(aes(color = default), size = 1)

plot_ly(x=new$Dim.1, y=new$Dim.2, z=new$Dim.3, type="scatter3d", mode="markers", color=new$default)


## get test data 
res.mfa2 <- MFA(test[, 2:24], group = c(1, 3, 1, 6, 6, 6), type = c("s", "n", "s", "n", "s", "s"), 
               name.group = c("given credit", "demographic", "age", "history payment status", "bill statement", "previous payment" ),
               graph = FALSE)

ind2 <- get_mfa_ind(res.mfa2)
ind2$coord

newtest = ind2$coord[, 1:3]


#fviz_mfa_ind(res.mfa, col.ind = "cos2", 
       #      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
        #     repel = TRUE)
# this one takes forever. 

newtrain <- SMOTE(default ~ ., as.data.frame(train), perc.over = 500, perc.under = 100)
table(newtrain$default)

## classification
## LDA

Model1 <- train(default ~ Dim.1 + Dim.2 + Dim.3, data = new, method = "lda")
Pred1 <- predict(Model1, newdata = newtest, type = "prob")
a <- Pred1[,2]/Pred1[,1]
table(test$default)

y <- test$default
g <- roc(y ~ a)
plot(g, grid = TRUE, print.auc = TRUE)

Pred12 <- predict(Model1, newdata = new, type = "prob")
a12 <- Pred12[,2]/Pred12[,1]
                         
g <- roc(y2 ~ a12)
plot(g, grid = TRUE, print.auc = TRUE)

## AUC = 0.687

Model2 <- train(default ~ Dim.1 + Dim.2 + Dim.3, data = new, method = "qda")
Pred2 <- predict(Model2, newdata = newtest, type = "prob")
a2 <- Pred2[,2]/Pred2[,1]

y <- test$default
g <- roc(y ~ a2)
plot(g, grid = TRUE, print.auc = TRUE)

Pred22 <- predict(Model2, newdata = new, type = "prob")
a22 <- Pred22[,2]/Pred22[,1]

g <- roc(y2 ~ a22)
plot(g, grid = TRUE, print.auc = TRUE)

## AUC = 0.712

Model3 <- train(default ~ Dim.1 + Dim.2 + Dim.3, data = new, method = "glm")
Pred3 <- predict(Model3, newdata = newtest, type = "prob")
a3 <- Pred3[,2]/Pred3[,1]

y <- test$default
g <- roc(y ~ a3)
plot(g, grid = TRUE, print.auc = TRUE)

Pred32 <- predict(Model3, newdata = new, type = "prob")
a32 <- Pred32[,2]/Pred32[,1]

g <- roc(y2 ~ a32)
plot(g, grid = TRUE, print.auc = TRUE)
##AUC = 0.705

## KNN
# use the cross validation to choose k
set.seed(2021)
ctrl <- trainControl(method="repeatedcv",number = 5, repeats = 3)
knnFit <- train(default ~ Dim.1 + Dim.2 + Dim.3, data = new, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
knnFit

## k = 27
Model4 <- train(default ~ Dim.1 + Dim.2 + Dim.3, data = new, method = "knn", tuneGrid = data.frame(k=c(27)))
Pred4 <- predict(Model4, newdata = newtest, type = "prob")
a4 <- Pred4[,2]/Pred4[,1]

g <- roc(y ~ a4)
plot(g, grid = TRUE, print.auc = TRUE)

Pred42 <- predict(Model4, newdata = new, type = "prob")
a42 <- Pred42[,2]/Pred42[,1]

g <- roc(y2 ~ a42)
plot(g, grid = TRUE, print.auc = TRUE)

## AUC = 0.705

## SVM
ctrl2 <- trainControl(method="repeatedcv",   
                     repeats=3,    
                     number = 5,  
                     summaryFunction = twoClassSummary,
                     classProbs=TRUE)


#Train and Tune the SVM
levels(new$default) <- c("X0", "X1")
svm.tune <- train(default ~ Dim.1 + Dim.2 + Dim.3, data = new, 
                  trControl = ctrl2,
                  method = "svmLinear",
                  tuneLength= 5,
                  preProc = c("center", "scale"),
                  metric = "ROC")
svm.tune

Model5 <- train(default ~ Dim.1 + Dim.2 + Dim.3, data = new, method = "svmLinear" )
Pred5 <- predict(svm.tune, newdata = newtest, type = "prob")
a5 <- Pred5[,2]/Pred5[,1]

g <- roc(y ~ a5)
plot(g, grid = TRUE, print.auc = TRUE)

Pred52 <- predict(svm.tune, newdata = new, type = "prob")
a52 <- Pred52[,2]/Pred52[,1]

g <- roc(y2 ~ a52)
plot(g, grid = TRUE, print.auc = TRUE)

### AUC = 0.675

### Random forest
#levels(new$default) <- c(0, 1)
# tuning the parameters for random forest
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        search = 'random')

mtry <- sqrt(3)

levels(new$default) <- c("X0", "X1")
rf_default <- train(default~ Dim.1 + Dim.2 + Dim.3, data = new, 
                    method='rf', 
                    metric='AUC', 
                    tuneLength = 15, 
                    trControl=control)

Model6 = randomForest(default~ Dim.1 + Dim.2 + Dim.3, data = new, 
                  mtry = 1, importance = TRUE, ntree=2500)
Pred6 <- predict(Model6, newdata = newtest, type = "prob")
Pred62 <- predict(Model6, newdata = new, type = "prob")
a6 <- Pred6[,2]/Pred6[,1]
a62 <- Pred62[,2]/Pred62[,1]
y2 <- new$default
g <- roc(y2 ~ a62)
plot(g, grid = TRUE, print.auc = TRUE)
g <- roc(y ~ a6)
plot(g, grid = TRUE, print.auc = TRUE)

control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        search = 'grid')

tunegrid <- expand.grid(.mtry = c(sqrt(ncol(new))))
modellist <- list()

for (ntree in c(1000,1500,2000,2500)){
  set.seed(123)
  fit <- train(default~ Dim.1 + Dim.2 + Dim.3,
               data = new,
               method = 'rf',
               metric = 'Accuracy',
               tuneGrid = tunegrid,
               trControl = control,
               ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

#Compare results
results <- resamples(modellist)
summary(results)

## classification tree
set.seed(12345)
tree1 <- tree(default ~ Dim.1 + Dim.2 + Dim.3, data = new)
plot(tree1)
text(tree1, cex = 1.1)

cv.tree1 <- cv.tree(tree1)
plot(cv.tree1$size, cv.tree1$dev, type = "b")

prune.tree1 = prune.tree(tree1, best = 3)
prune.tree1

Model7 <- rpart(default ~ Dim.1 + Dim.2 + Dim.3, data = new)
plot(Model7)
text(Model7)

newtest = as.data.frame(newtest)
Pred7 <- predict(Model7, newdata = newtest, type = "prob")
a7 <- Pred7[,2]/Pred7[,1]

g <- roc(y ~ a7)
plot(g, grid = TRUE, print.auc = TRUE)

Pred72 <- predict(Model7, newdata = new, type = "prob")
a72 <- Pred72[,2]/Pred72[,1]

g <- roc(y2 ~ a72)
plot(g, grid = TRUE, print.auc = TRUE)
## Boost tree

Model8 <- gbm(as.character(default) ~ Dim.1 + Dim.2 + Dim.3, data = new, distribution = "bernoulli", 
              n.trees = 2500, interaction.depth = 2, verbose = FALSE, cv.folds = 5, shrinkage = 0.01)

Pred8 <- predict(Model8, newdata = newtest, n.trees = 2500, type = "response")

g <- roc(y ~ Pred8)
plot(g, grid = TRUE, print.auc = TRUE)

Pred82 <- predict(Model8, newdata = new, n.trees = 2500, type = "response")

g <- roc(y2 ~ Pred82)
plot(g, grid = TRUE, print.auc = TRUE)

roc_rose <- plot(roc(test$default, Pred8),  col = "blue")  #0.700 
roc_rose <- plot(roc(test$default, a2),  col = "green", add = TRUE) #qda 0.693
roc_rose <- plot(roc(test$default, a3),  col = "black", add = TRUE) #logistic 0.687
roc_rose <- plot(roc(test$default, a),  col = "red", add = TRUE)  #lda 0.687
roc_rose <- plot(roc(test$default, a4),  col = "gray", add = TRUE) #knn 0.685
roc_rose <- plot(roc(test$default, a6),  col = "pink", add = TRUE) #random forest #0.678
#roc_rose <- plot(roc(test$default, a5),  col = "yellow", add = TRUE) #svm 0.672

legend(0.3,0.25,
       legend = c("BCT AUC: 0.700", 
                  "QDA AUC: 0.693", 
                  "LR  AUC: 0.687", 
                  "LDA AUC: 0.687", 
                  "KNN AUC: 0.685", 
                  "RF  AUC: 0.678"), 
       col=c("blue", "green", "black", "red", "gray", "pink"), lwd=7, cex=.7)
# plotLift(Pred1, labels = c(0,1), cumulative = TRUE)

roc_rose <- plot(roc(new$default, a62),  col = "blue")  #1 rf excellent  
roc_rose <- plot(roc(new$default, a42),  col = "green", add = TRUE) #knn 0.770
roc_rose <- plot(roc(new$default, Pred82), col = "black", add = TRUE) #bt 0.741
roc_rose <- plot(roc(new$default, a22),  col = "red", add = TRUE)  #qda 0.717
roc_rose <- plot(roc(new$default, a32),  col = "gray", add = TRUE) #glm 0.710
roc_rose <- plot(roc(new$default, a12),  col = "pink", add = TRUE) #lda #0.709
#roc_rose <- plot(roc(test$default, a5),  col = "yellow", add = TRUE) #svm 0.672

legend(0.3,0.25,
       legend = c("RF  AUC: 1.000", 
                  "KNN AUC: 0.770", 
                  "BCT AUC: 0.741", 
                  "QDA AUC: 0.717", 
                  "LR  AUC: 0.710", 
                  "LDA AUC: 0.709"), 
       col=c("blue", "green", "black", "red", "gray", "pink"), lwd=7, cex=.7)
# plotLift(Pred1, labels = c(0,1), cumulative = TRUE)
