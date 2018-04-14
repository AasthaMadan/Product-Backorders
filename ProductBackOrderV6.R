library(caret)
library(dplyr)
library(plyr)
library(ggplot2)
library(corrplot)
library(DMwR)
require(e1071)
library(glmnet)
library(MASS)
library(ROCR)
library(vegan)
library(car)
library(data.table)

# function to read csv file and save as rdata in desired folder location
Readcsv <- function(File,Inext,OutExt,path)
{
  path<-file.path(path,File)
  Inpath<-paste(path,Inext,sep="")
  Outpath<-paste(path,OutExt,sep="")
  prductsData <- read.csv(Inpath,na.strings=c("","NA"))
  saveRDS(prductsData, file = Outpath)
  prductsData<- prductsData
}

# Data Cleaning
DataCleaning<- function(PrductsData) {
  attach(PrductsData)
  PrductsData<-PrductsData[!duplicated(PrductsData), ]
  # remove the sku as all values are unique
  Unique <- rapply(PrductsData,function(x)length(unique(x)))
  drop <- c()
  for (i in 1:length(Unique))
  {
    if ((Unique[[i]])==nrow(PrductsData))
    {
      r=names(Unique[i])
      drops <- c(drop ,r)
      
    }   
  }
  PrductsData <- PrductsData[ , !(colnames(PrductsData) %in% drops)]
  
  # remove the rows with all NAs
  PrductsData<-PrductsData[apply(PrductsData, 1, function(y) !all(is.na(y))),]
  # remove the Cols with all NAs
  PrductsData<-PrductsData[sapply(PrductsData, function(x) !all(is.na(x)))]
  # drop unused levels induced by null row
  PrductsData <- droplevels(PrductsData)
}
# Data visualization
datavisualization <- function(PrductsData){
  # split data wrt data types , check dstribution and variance
  categoricalData<-select_if(PrductsData,is.factor)
  ## check distribution and variation and use box plots
  NumericData<-select_if(PrductsData,is.numeric)
  
  library(reshape2)
  train_melt = melt(PrductsData)
  ggplot(data = train_melt, aes(x=value, fill = variable)) + geom_histogram()+facet_grid(variable~.)
  
  
  attach(PrductsData)
  # Scatterplot with 5 dimensions (by adding color, shape, & size to the basic plot)
  ggplot(data=PrductsData, aes(x=went_on_backorder, y=national_inv, colour = lead_time, size = in_transit_qty)) + 
    geom_point() + 
    xlab("went_on_backorder")+ylab("national_inv") + 
    ggtitle("went_on_backorder Vs. national_inv")
  
  ggplot2::ggplot(PrductsData,aes(min_bank)) + geom_histogram(binwidth=5) +
    xlab("min_bank")
  
  par(mfrow=c(1,1))
  qplot(went_on_backorder,national_inv, geom = "boxplot",
        main = "went_on_backorder Vs. national_inv")
  
  boxplot(national_inv,main = "Box Plot:national_inv", ylab = "national_inv")
  skewness(PrductsData$national_inv)
  kurtosis(PrductsData$national_inv)
  
  ggplot(data=PrductsData, aes(PrductsData$perf_6_month_avg)) +
    geom_bar(col="blue")+
    geom_text(stat='bin',aes(label=..count..),vjust=-1)
  labs(title="Barplot for perf_6_month_avg", x="perf_6_month_avg", y="Count")
  ## Managing and grouping graphical gg objects to plot graphs
  p1 <- ggplot(data = PrductsData)
  p2 <- aes(x = sales_3_month, y = forecast_3_month)
  g1 <- p1 + p2 + geom_point(aes(color = factor(went_on_backorder))) #scatterplot with 3 dimensions
  ggsave(filename = "salesnForecastBO.png", plot = g1)
  
  p2 <- aes(x = min_bank, y = forecast_3_month)
  g2 <- p1 + p2 + geom_point(aes(color = factor(went_on_backorder))) #scatterplot with 3 dimensions
  ggsave(filename = "ForecastvsMinBank.png", plot = g2)
  
  p2 <- aes(x = local_bo_qty, y = national_inv)
  g3 <- p1 + p2 + geom_point(aes(color = factor(went_on_backorder))) #scatterplot with 3 dimensions
  ggsave(filename = "LocalvsNational.png", plot = g3)
  
  g5=qplot(potential_issue, data=PrductsData, geom="bar",
           fill = went_on_backorder,main="Distribution of Backorders for each Potential risk")
  ggsave(filename = "barPotentialissuevsBO.png", plot = g5)
  
  library(miscset) # install from CRAN if required
  g4<-ggplotGrid(ncol = 2,
                 lapply(c("potential_issue","deck_risk", "oe_constraint", "ppap_risk", "stop_auto_buy", "rev_stop","went_on_backorder"),
                        function(col) {
                          ggplot(PrductsTrainData, aes_string(col)) + geom_bar() + coord_flip()
                        }))
  ggsave(filename = "CatogoryDist.png", plot = g4)
  
  library(tidyr)
  df2 <- PrductsData %>% 
    group_by(stop_auto_buy, went_on_backorder) %>% 
    tally() %>% 
    complete(went_on_backorder, fill = list(n = 0)) %>% 
    mutate(percentage = n / sum(n) * 100)
  
  g6<-ggplot(df2, aes(went_on_backorder, percentage, fill =stop_auto_buy)) + 
    geom_bar(stat = 'identity', position = 'dodge') +
    theme_bw()
  ggsave(filename = "stpAutoBuyvsBO.png", plot = g6)
  
  categoricalData<-select_if(PrductsData,is.factor)
  freq_tables = apply(categoricalData,2, table)
  print(freq_tables)
}

#Data imputation
DataImputation<- function(PrductsData) {
  attach(PrductsData)
  # impute lead_time NA values with mean
  PrductsData$lead_time[is.na(PrductsData$lead_time)] = mean(PrductsData$lead_time, na.rm=TRUE)
  
  PrductsData$perf_12_month_avg[PrductsData$perf_12_month_avg==-99] <- NA
  #Use rpart:
  library(rpart)
  
  imputedds <- rpart(perf_12_month_avg ~ .,
                     data = PrductsData[!is.na(PrductsData$perf_12_month_avg),-1],
                     method = "anova")
  imputedds$cptable
  PrductsData$perf_12_month_avg[is.na(PrductsData$perf_12_month_avg)] <- predict(imputedds, PrductsData[is.na(PrductsData$perf_12_month_avg),])
  names(PrductsData)[names(PrductsData)=="PrductsData$went_on_backorder"] <- "went_on_backorder"
  PrductsData<-PrductsData
  PrductsData$perf_6_month_avg[PrductsData$perf_6_month_avg==-99] <- NA
  
  imputedds <- rpart(perf_6_month_avg ~ .,
                     data = PrductsData[!is.na(PrductsData$perf_6_month_avg),-1],
                     method = "anova")
  imputedds$cptable
  PrductsData$perf_6_month_avg[is.na(PrductsData$perf_6_month_avg)] <- predict(imputedds, PrductsData[is.na(PrductsData$perf_6_month_avg),])
  
  
  names(PrductsData)[names(PrductsData)=="PrductsData$went_on_backorder"] <- "went_on_backorder"
  PrductsData<-PrductsData
  
}
# Data transformation
#Function to convert Yes/No to 1/0
Mapvalue <- function(x) {
  mapvalues(x, from = c("Yes", "No"), to = c(1,0))
}
dataTransformations <-function(PrductsData){
  categoricalData<-select_if(PrductsData,is.factor)
  categoricalData[c(1:ncol(categoricalData))] <- lapply(categoricalData[c(1:ncol(categoricalData))], Mapvalue)
  NumericData<-select_if(PrductsData,is.numeric)
  PrductsData <- cbind(NumericData, categoricalData)
}
dataTransformationNumeric <-function(PrductsData)
{
  categoricalData<-select_if(PrductsData,is.factor)
  categoricalData[c(1:ncol(categoricalData))] <- lapply(categoricalData[c(1:ncol(categoricalData))], factor)
  indx <- sapply(categoricalData, is.factor)
  categoricalData[indx] <- lapply(categoricalData[indx], function(x) as.numeric(as.character(x)))
  NumericData<-select_if(PrductsData,is.numeric)
  PrductsData <- cbind(NumericData, categoricalData)
}

#Data Normalization
dataNormalization <-function(PrductsData){
  preProc<-preProcess(PrductsData[,setdiff(names(PrductsData),"went_on_backorder")])
  PrductsData<-predict(preProc,PrductsData)
  names(PrductsData)[names(PrductsData)=="PrductsData$went_on_backorder"] <- "went_on_backorder"
  PrductsData<- PrductsData
}
# Data Reduction
DataReduction<- function(PrductsData) {
  attach(PrductsData)
  dt<-as.data.table(PrductsData)
  # remove the rows where the forecast and sales both are 0 and the product did not go to back order.
  PrductsData<-data.frame(dt[forecast_3_month!=0 | sales_3_month!=0 & went_on_backorder=="0"])
  names(PrductsData)[names(PrductsData)=="PrductsData$went_on_backorder"] <- "went_on_backorder"
  PrductsData <- PrductsData
}
#Feature selection using correlations
FeatureSelection1 <-function(PrductsData){
  attach(PrductsData)
  # split data wrt data types , check dstribution and variance
  categoricalData<-select_if(PrductsData,is.factor)
  df2 <- t(round(cbind(apply(categoricalData, 2, function(x) {
    ch <- chisq.test(categoricalData$went_on_backorder, x)
    c(unname(ch$statistic), ch$parameter, ch$p.value )})), 3))
  print(df2)
  NumericData<-select_if(PrductsData,is.numeric)
  cor.test(PrductsData$perf_6_month_avg, PrductsData$perf_12_month_avg, method=c("pearson"))
  features<- subset(PrductsData, select=-c(went_on_backorder))
  descrCor <- cor(NumericData)
  highlyCorDescr <- findCorrelation(descrCor, cutoff = .80)
  print(highlyCorDescr)
  
  Processed_data <- cbind(NumericData[,-highlyCorDescr], categoricalData)
}

RunRF_VIF<- function(PrductsData)
{
  library(randomForest)
  model_rf <- randomForest(went_on_backorder~., data=PrductsData, ntree=50, mtry=10)
  VI_F<-importance(model_rf)
  barplot(t(VI_F/sum(VI_F)),space = 1, horiz = TRUE, las = 1, cex.names = 0.8)
  importanceOrder=order(-model_rf$importance)
  log_reg <- glm (went_on_backorder~., data = PrductsData, family = binomial)
  Step1 <- stepAIC(log_reg, direction="backward")
  vifInfo <- vif(log_reg)
  vifInfo<-cbind(as.data.table(vifInfo),as.data.table(VI_F))
  vif<-as.data.frame(vifInfo)
  
}

#Feature selection using RF
FeatureSelection2 <-function(vifInfo,I=0){
  vif<-as.data.frame(vifInfo)
  if(I==0)
  {
    importanceOrder<-vif[with(vif, order(-vifInfo)), ]
  }
  else
  {
    importanceOrder<-vif[with(vif, order(MeanDecreaseGini)), ]
  }
  
}

CreateFeatureSet1 <- function(PrductsData){
  Normalized<- dataNormalization(PrductsData)
}
CreateFeatureSet2 <- function(PrductsData){
  DataReduced<- DataReduction(PrductsData)
  Normalized<- dataNormalization(DataReduced)
}
CreateFeatureSet3 <- function(PrductsData){
  DimReduced <- FeatureSelection1(PrductsData)
  Normalized<- dataNormalization(DimReduced)
}
CreateFeatureSet4 <- function(PrductsData){
  DataReduced<- DataReduction(PrductsData)
  DimReduced <- FeatureSelection1(DataReduced)
  Normalized<- dataNormalization(DimReduced)
}
CreateFeatureSet5 <- function(PrductsData,VIF_Imp_matrix){
  
  VIF_Imp_matrix <-FeatureSelection2(VIF_Imp_matrix,0)
  print(VIF_Imp_matrix)
  a <- which(VIF_Imp_matrix$vifInfo>4,arr.ind=T)
  
  if(length(a)==0)
  {
    return(NULL)
  }
  else
  {
    PrductsData <- PrductsData[-c(as.integer(row.names(VIF_Imp_matrix)[1]))]
  }
  
}
CreateFeatureSet6 <- function(PrductsData,VIF_Imp_matrix){
  VIF_Imp_matrix <- FeatureSelection2(VIF_Imp_matrix,1)
  print(VIF_Imp_matrix)
  PrductsData <- PrductsData[-c(as.integer(row.names(VIF_Imp_matrix)[1]))]
}
# No normalization!!
CreateFeatureSet7 <- function(PrductsData){
  DataReduced<- DataReduction(PrductsData)
  print(str(DataReduced))
  DimReduced <- FeatureSelection1(DataReduced)
  return(data.frame(DimReduced[2]))
  
}

BalanceDataSmote <- function(PrductsData,I=0){
  
  library(unbalanced)
  PrductsData<-dataTransformations(PrductsData)
  n<-ncol(PrductsData)
  output<-PrductsData$went_on_backorder
  input<-PrductsData[ ,-n]
  BalancedTrain <- ubBalance(input, output, type="ubSMOTE", positive="1", percOver=300, percUnder=300, k=5, verbose=FALSE)
  BalancedTrain$Y <- as.factor(as.character(BalancedTrain$Y))
  balancedTrainData<-cbind(BalancedTrain$X,BalancedTrain$Y)
  names(balancedTrainData)[names(balancedTrainData)=="BalancedTrain$Y"] <- "went_on_backorder"
  balancedTrainData<-balancedTrainData
  
}

BalanceDataUnder <- function(PrductsData, I=0){
  
  library(unbalanced)
  PrductsData<-dataTransformations(PrductsData)
  print(str(PrductsData))
  n<-ncol(PrductsData)
  print(n)
  output<-PrductsData[ ,n]
  print(str(output))
  input<-PrductsData[ ,-n]
  print(str(input))
  BalancedTrain <- ubUnder(X=input, Y= output, perc = 50,method = "percPos")
  print(str(BalancedTrain))
  BalancedTrain$Y <- as.factor(as.character(BalancedTrain$Y))
  print(str(BalancedTrain$Y))
  balancedTrainData<-cbind(BalancedTrain$X,BalancedTrain$Y)
  print(str(balancedTrainData))
  names(balancedTrainData)[names(balancedTrainData)=="BalancedTrain$Y"] <- "went_on_backorder"
  balancedTrainData<-balancedTrainData
}

DataPartition <- function(PrductsData){
  set.seed(7)
  trainRows=createDataPartition(PrductsData[,ncol(PrductsData)],p=0.8,list = F)
  train = as.data.frame(PrductsData[trainRows,])
  Validate = as.data.frame(PrductsData[-trainRows,])
  return(list(train,Validate))
}

AUCgrid<-function(Confmat,strng){
  valTPR=Confmat$byClass[[1]]
  valFPR=1-Confmat$byClass[[2]]
  valAUC=(valTPR-valFPR +1)/2
  return(list(strng,valAUC))
}


## Main Block----------------
# Read train and test data-------
RawTrainData <- Readcsv("Kaggle_Training_Dataset_v2", ".csv",".RData", "C:/Users/Aastha/Documents/INSOFE/Project/Version2")
RawTestData <- Readcsv("Kaggle_Test_Dataset_v2", ".csv",".RData", "C:/Users/Aastha/Documents/INSOFE/Project/Version2")
#--- understand the structure of train data----
dim(RawTrainData)
glimpse(RawTrainData)
head(RawTrainData)
tail(RawTrainData)

#--- understand the structure of test data----
dim(RawTestData)
glimpse(RawTestData)
head(RawTestData)
tail(RawTestData)

# Data understanding
# •	Class imbalance (Only .67% volume of products went on back order)
# •	lead_time has NA values 
# •	Last Row has all NA values
# •	Remove sku (all unique values)
# •	perf_6_month_avg, perf_12_month_avg attributes have missing data with -99 values.
# •	SKUs for which forecast and sales are 0 and the target class “Went to Back order is also “No”. 



#--- Cleanse the train data--
PrductsTrainData<-DataCleaning(RawTrainData)
#-- check the structure of cleansed data---
glimpse(PrductsTrainData)
#--- Impute the train data---
Imputed<- DataImputation(PrductsTrainData)
# data visualization
datavisualization(PrductsTrainData)

FeatureSet1 <- CreateFeatureSet1(Imputed)
FeatureSet2 <- CreateFeatureSet2(Imputed)
FeatureSet3 <- CreateFeatureSet3(Imputed)
FeatureSet4 <- CreateFeatureSet4(Imputed)

#----------balancing data-----------
if(!is.null(FeatureSet3))
{
  OverSampledData3<-BalanceDataSmote(FeatureSet3,3)
  UnderSampledData3<-BalanceDataUnder(FeatureSet3,3)
}
if(!is.null(FeatureSet4))
{
  OverSampledData4<-BalanceDataSmote(FeatureSet4,4)
  UnderSampledData4<-BalanceDataUnder(FeatureSet4,4)
}

# #---- Test Data cleaning and initial preparation----------------
PrductsTestData<-DataCleaning(RawTestData)
PrductsTestData<- DataImputation(PrductsTestData)
PrductsTestData<-dataTransformations(PrductsTestData)
PrductsTestData1<-PrductsTestData[,-ncol(PrductsTestData)]
PrductsTestData1<-dataNormalization(PrductsTestData1)

#----------Run Models on feature set 3 balanced with Smote----------------
#---Use Cross validation for robust model
control <- trainControl(method="repeatedcv", number=5, repeats=3)
set.seed(7)
#---1. Logistic regression------------
fit.glm1 <- caret::train(went_on_backorder~., data=OverSampledData3, method="glm", trControl=control)
varImp(fit.glm1)
fit.glm1$results

set.seed(7)
#---2. Decision Tree------------
fit.cart1 <- caret::train(went_on_backorder~., data=OverSampledData3, method="rpart", trControl=control)
varImp(fit.cart1)

set.seed(7)
#---3. Random forest (ranger as variant in caret) ------------
fit.rf1 <- caret::train(went_on_backorder~., data=OverSampledData3, method="ranger", trControl=control)

set.seed(7)
#---4. GBM------------
fit.gbm1 <- caret::train(went_on_backorder~., data=OverSampledData3, method="gbm", trControl=control,verbose = FALSE)

set.seed(7)
#---5. XGBoost------------
fit.xgb1 <- caret::train(went_on_backorder~., data=OverSampledData3, method="xgbTree", trControl=control,verbose = FALSE)

results1 <- resamples(list(GLM=fit.glm1,CART=fit.cart1, GBM=fit.gbm1, ranger=fit.rf1, XGB=fit.xgb1))

summary(results1)


#--First Take:  Model comparison Box plot for Accuracy and Kappa -----
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results1, scales=scales)


#--Important metric in this case is FScore or AUC and not accuracy. Train models for other feature sets and compare----

#---- First Take: Predict on test data ------
#-- Confusion matrix of train data for logistic regression
caret::confusionMatrix(OverSampledData3$went_on_backorder,predict(fit.glm1),positive = "1")

test.glm <- predict(fit.glm1, newdata = PrductsTestData1)
#-- Confusion matrix of test data for logistic regression
ConfGLM<-confusionMatrix(data = test.glm , reference =PrductsTestData$went_on_backorder ,positive = "1")
#-- Confusion matrix of train data for Decision tree
caret::confusionMatrix(OverSampledData3$went_on_backorder,predict(fit.cart1),positive = "1")
test.CART <- predict(fit.cart1, newdata = PrductsTestData1)
#-- Confusion matrix of test data for Decision tree
ConfDT<-confusionMatrix(data = test.CART , reference =PrductsTestData$went_on_backorder ,positive = "1")
#-- Confusion matrix of train data for random forest
caret::confusionMatrix(OverSampledData3$went_on_backorder,predict(fit.rf1),positive = "1")
test.RF <- predict(fit.rf1, newdata = PrductsTestData1)
#-- Confusion matrix of test data for Random Forest
ConfRF<-confusionMatrix(data = test.RF , reference =PrductsTestData$went_on_backorder ,positive = "1")
#-- Confusion matrix of train data for GBM
caret::confusionMatrix(OverSampledData3$went_on_backorder,predict(fit.gbm1),positive = "1")
test.GBM <- predict(fit.gbm1, newdata = PrductsTestData1)
#-- Confusion matrix of test data for GBM
ConfGBM<-confusionMatrix(data = test.GBM , reference =PrductsTestData$went_on_backorder ,positive = "1")
#-- Confusion matrix of train data for XGBoost
caret::confusionMatrix(OverSampledData3$went_on_backorder,predict(fit.xgb1),positive = "1")
test.XGB <- predict(fit.xgb1, newdata = PrductsTestData1)
#-- Confusion matrix of test data for XGBoost
ConfXGB<-confusionMatrix(data = test.XGB , reference =PrductsTestData$went_on_backorder ,positive = "1")

#Compare the Results of all the models
ResultsConf<-cbind(ConfGLM$byClass,ConfDT$byClass)
ResultsConf<-cbind(ResultsConf,ConfRF$byClass)
ResultsConf<-cbind(ResultsConf,ConfGBM$byClass)
ResultsConf<-cbind(ResultsConf,ConfXGB$byClass)
print(ResultsConf)

# Calculate AUC for Test and compare for all the models
AUCTable<-AUCgrid(ConfGLM,"GLM")
AUCTable<- cbind(AUCTable,AUCgrid(ConfDT,"DT"))
AUCTable<- cbind(AUCTable,AUCgrid(ConfRF,"RF"))
AUCTable<- cbind(AUCTable,AUCgrid(ConfGBM,"GBM"))
AUCTable<- cbind(AUCTable,AUCgrid(ConfXGB,"XGB"))
print(AUCTable)
