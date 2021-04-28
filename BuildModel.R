#################---------Data Cleaning------------------#####################
# Reading in data
setwd("C:/Users/Akshay/Downloads")
occdata <- read.csv("CSVDownload.csv")
#occdata <- read.csv("D:/Docs/cmdaCap/eddData/CSVDownload.csv")
# Splitting into states column
occdata$State <- gsub("^.+?, |, United States", "", occdata$Location)
# Filtering to only Virginia, NC
library(dplyr)
#occdata <- occdata %>% filter(State=="Virginia" | State=="North Carolina" | State="South Carolina")
# Decide which columns to keep
keep_cols = c("ObsDate", "Latitude", "Longitude")
occdata <- occdata[keep_cols]
print(length(occdata$ObsDate))
# Filtering out NAs and Blanks
occdata <- occdata[!is.na(occdata$Latitude), ]
print(length(occdata$ObsDate))
occdata <- occdata[!occdata$ObsDate == "", ]
print(length(occdata$ObsDate))
# Coverting all dates to the same way
# occdata$ObsDate <- as.Date(occdata$ObsDate, "%d-%b-%y")
occdata$occurence <- 1



###################-------------------Generate background/absence data----------------#######################
library(raster)
library(rgeos)
library(dismo)
# Using ours
set.seed(99)
attach(occdata)
#converts to raster
bioData = getData('worldclim', var='bio', res=10)
coordinates(occdata) <- ~Longitude+Latitude
projection(occdata) <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84')
mask <- bioData

# radius of 16 km
#install packages rgeos
x <- circles(occdata, d=80000, lonlat=TRUE)
pol <- polygons(x)
samp1 <- spsample(pol, 2500, type="random", iter=25)
#mask <- raster(occdata)
cells <- cellFromXY(mask, samp1)
# 250 unique points
length(cells)
cells
cells <- unique(cells)
length(cells)
xy <- xyFromCell(mask, cells)
par(mar=c(1,1,1,1))
plot(pol, xlab="Longitude", ylab="Latitude", main="Pseudo-Absence Points")
points(xy, ces=0.75, pch=20, col="blue")
# Removing NAs
library(rgr)
xy <- remove.na(xy, iftell=FALSE)
spxy <- SpatialPoints(xy, proj4string=CRS('+proj=longlat +datum=WGS84'))
o <- over(spxy, geometry(x))
xyInside <- xy[["x"]][!is.na(o), ]
absdf <- data.frame(NA, xyInside[,2], xyInside[,1], 0)
absdf
# rerun to recreate occdata back to df
#occdata <- read.csv("D:/Docs/cmdaCap/eddData/CSVDownload.csv")
occdata <- read.csv("CSVDownload.csv")
# Splitting into states column
occdata$State <- gsub("^.+?, |, United States", "", occdata$Location)
# Filtering to only Virginia
#occdata <- occdata %>% filter(State=="Virginia" | State=="North Carolina" | State=="South Carolina")
# Decide which columns to keep
keep_cols = c("ObsDate", "Latitude", "Longitude")
occdata <- occdata[keep_cols]
print(length(occdata$ObsDate))
# Filtering out NAs and Blanks
occdata <- occdata[!is.na(occdata$Latitude), ]
occdata <- occdata[!occdata$ObsDate == "", ]
# Coverting all dates to the same way
# occdata$ObsDate <- as.Date(occdata$ObsDate, "%d-%b-%y")
occdata$occurence <- 1

names(absdf) <- c("ObsDate", "Latitude", "Longitude", "occurence")
occdata <- rbind(occdata, absdf)

# Add in about 20 or so lat and longs with occurence as 0 for North/South Dakota, Wyoming, and Montana (4 states with no reported observations)
#https://www.mapsofworld.com/usa/states/montana/lat-long.html has listed lats and longs for cities in the state
latND = c(48, 48, 48, 48, 48, 48, 48, 47, 47, 47, 47, 47, 47, 47)
latSD = c(45, 45, 45, 45, 45, 45, 45, 45, 44, 44, 44, 44, 44, 44, 44, 44)
latMT <- c(48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
           47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47,
           46, 46, 46, 46, 46, 46, 46, 46, 46, 46,
           45, 45)
latWY <- c(45, 45, 45, 45, 45, 45, 45,
           44, 44, 44, 44, 44, 44, 44,
           43, 43, 43, 43, 43, 43, 43,
           42, 42, 42, 42, 42, 42, 42,
           41, 41, 41, 41, 41, 41, 41)
longND = c(104, 103, 102, 101, 100, 99, 98, 104, 103, 102, 101, 100, 99, 98)
longND <- -(longND)
longSD = c(104, 103, 102, 101, 100, 99, 98, 97, 104, 103, 102, 101, 100, 99, 98, 97)
longSD <- -(longSD)
longMT <- c(116, 115, 114, 113, 112, 111, 110, 109, 108, 107, 106, 105,
            115, 114, 113, 112, 111, 110, 109, 108, 107, 106, 105,
            114, 113, 112, 111, 110, 109, 108, 107, 106, 105,
            113, 112)
longMT <- -(longMT)
longWY <- c(111, 110, 109, 108, 107, 106, 105,
            111, 110, 109, 108, 107, 106, 105,
            111, 110, 109, 108, 107, 106, 105,
            111, 110, 109, 108, 107, 106, 105,
            111, 110, 109, 108, 107, 106, 105)
longWY <- -(longWY)

lat <- c(latND, latSD, latMT, latWY)
long <- c(longND, longSD, longMT, longWY)
newdf <- data.frame(NA,lat,long,0)
names(newdf)<-c("ObsDate","Latitude","Longitude","occurence")
occdata <- rbind(occdata, newdf)


# remove duplicate rows
library(dplyr)
occdata <- occdata %>% distinct(Latitude,Longitude, .keep_all=TRUE)
length(occdata$ObsDate)

library(ggplot2)
library(maps)
occdata$occurence <- as.factor(occdata$occurence)
m = map_data('state', region = 'Virginia')
ggplot() +
  geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=occdata,aes(x=Longitude,y=Latitude, color=occurence))+
  ggtitle("Distribution of Reported Sightings of Tree of Heaven in Virginia")+
  xlab('Longitude')+
  ylab('Latitude')+
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed()

#####################------------------------Getting the enivornmental data and merging to a df----------------------############################
xy <- occdata[,c(3,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data = occdata,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

#bioData = getData('worldclim', var='bio', res=0.5, lon=-77.24461, lat=38.69997)
bioData = getData('worldclim', var='bio', res=10)
altData = getData('worldclim', var='alt', res=10)
?getData
# extract the data and put into temp var
biodat <- extract(bioData, spdf)
altdat <- extract(altData, spdf)

# combine spatial coords and biodata
dfBio <- cbind.data.frame(coordinates(spdf),biodat)
dfAlt <- cbind.data.frame(coordinates(spdf),altdat)

# merge obs
dfMerged <- merge(occdata, dfBio, by=c("Longitude","Latitude"))
dfMerged <- merge(dfMerged, dfAlt, by=c("Longitude","Latitude"))

# BIO1 = Annual Mean Temperature

# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))

# BIO3 = Isothermality (BIO2/BIO7) (×100)

# BIO4 = Temperature Seasonality (standard deviation ×100)
#
# BIO5 = Max Temperature of Warmest Month
#
# BIO6 = Min Temperature of Coldest Month
#
# BIO7 = Temperature Annual Range (BIO5-BIO6)
#
# BIO8 = Mean Temperature of Wettest Quarter
#
# BIO9 = Mean Temperature of Driest Quarter
#
# BIO10 = Mean Temperature of Warmest Quarter
#
# BIO11 = Mean Temperature of Coldest Quarter
#
# BIO12 = Annual Precipitation
#
# BIO13 = Precipitation of Wettest Month
#
# BIO14 = Precipitation of Driest Month
#
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
#
# BIO16 = Precipitation of Wettest Quarter
#
# BIO17 = Precipitation of Driest Quarter
#
# BIO18 = Precipitation of Warmest Quarter
#
# BIO19 = Precipitation of Coldest Quarter


###############------------------Split dataset into train test/Check Assumptions------------------------#########################
# 70-30% sample
library(caTools)
drops <- c("ObsDate")
dfMerged <- dfMerged[ , !(names(dfMerged) %in% drops)]
dfMerged <- na.omit(dfMerged)

# Check for collinearity
round(cor(dfMerged), 2)
# (1,5) (1,6) (5,6) (1,8) (5,8) (6,8) (1,alt) (5,alt) (8,alt) are strongly correlated)

train_rows = sample.split(dfMerged$occurence, SplitRatio=0.7)
train = dfMerged[ train_rows,]
test  = dfMerged[!train_rows,]
y_test <- as.factor(test$occurence)
x_test <- test






#####################---------------------------GLM Model----------------------#################################
library(car)
modelGLM <- glm(as.factor(occurence)~bio1+bio2+bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+altdat, data=train,
                family="binomial")


plot(modelGLM$fitted.values, train$bio20)



# GLMNET using LASSO 
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
library(glmnet)
drops <- c("Longitude", "Latitude", "occurence")
train2 <- train[ , !(names(train) %in% drops)]
X <- data.matrix(train2)
Y <- data.matrix(train$occurence)
test2 <- x_test[ , !(names(x_test) %in% drops)]
Xtest <- data.matrix(test2)
Ytest <- data.matrix(y_test)

# Logistic
Logfit <- glmnet(X, Y, family = "binomial")
plot(Logfit, xvar = "dev", label = TRUE)
# Lambda 0.000003 is the lowest in our fit of models
LogResults <- predict(Logfit, newx = Xtest, type = "class", s = c(0.000003))
Logfit



# Gaussian
Gausfit <- cv.glmnet(X, Y, family="binomial", type.measure="class")
plot(Gausfit, xvar = "dev", label = TRUE)
Gausresults <- predict(Gausfit, newx = Xtest, s = "lambda.min", type = "class")

coef(Logfit)
coef(Gausfit)





# Conclusion: No good linear relationships with the probabilites, also no transformations would seem to help


###################----------------------------SVM Model------------------------##################################
# rbf/radial is nonlinear kernel
library(e1071)
modelSVM <- svm(formula=as.factor(occurence)~bio1+bio2+bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+altdat, data=train,
                type="C-classification", kernel="radial")


set.seed(123)
ctrl <- trainControl(method="cv",
                     number = 2,
                     summaryFunction=twoClassSummary,
                     classProbs=TRUE)

# Grid search to fine tune SVM
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)

#Train SVM
train5 <- train
train5 <- train5 %>% 
  mutate(occurence = factor(occurence, 
                        labels = make.names(levels(svm.Y))))
svm.tune <- train(x=train5[,4:23],
                  y=train5[,3],
                  method = "svmRadial",
                  metric="ROC",
                  tuneGrid = grid,
                  trControl=ctrl)

svm.tune

modelSVM <- svm.tune

# Conclusion: Pretty good fit using the non linear kernel (should be a non-parametric technique)

###################---------------------------CT Model-----------------------------#########################################
library(rpart)

# Regular CT
modelCT <- rpart(formula=as.factor(occurence)~bio1+bio2+bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+altdat, data=train, method="class") #control = rpart.control(minsplit=?,cp=?) sets min obs in a node to be 30 before attempting a split and a split must decrease the overall lack of fit by a factor of 0.001 before it is attempted
summary(modelCT)
printcp(modelCT)

# Boosted CT with XG Boost
library(xgboost)
library(caret)
library(Matrix)
set.seed(123)
class(train)

# This was used to determine best tuning parameters
# Note this takes around 5-10 min to run

# Using cross validation with 10 folds to find parameter values

# Coverting longs/lats to geohash first
#library(geohashTools)

#train$geohash <- gh_encode(train$Latitude, train$Longitude, precision=1L)
#modelCTBoost <- train(as.factor(occurence)~bio1+bio2+bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+bio13+bio14+bio15+bio16+bio17+bio18+bio19+altdat, data=train, 
 #                      method = "xgbTree", 
  #                     trControl = trainControl("cv", number=10))
 #Get best tuning parameters set
#modelCTBoost$bestTune

# What this returned

#nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#150         3     0.4  0              0.6                1         0.75

# XGBoost model with chosen parameters for a xgboost object
#train$geohash <- as.factor(train$geohash)
#train.y <- as.numeric(as.factor(train$occurence))-1
#sparse_matrix <- sparse.model.matrix(train.y~bio1+bio2+bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+
                                       #bio13+bio14+bio15+bio16+bio17+bio18+bio19+altdat, data = train)[,-1]
xgmatrix = xgb.DMatrix(data=as.matrix(train[,4:23]), label=as.numeric(as.factor(train$occurence))-1) 
modelCTBoost <- xgboost(data=xgmatrix, max.depth = 3, eta = 0.4, nrounds = 150, gamma=0, colsample_bytree=0.6, 
                        min_child_weight=1, subsample=0.75, objective = "binary:logistic", verbose=2)





# XGBoost model with chosen parameters for a caret object
#modelCTBoost <- train(as.factor(occurence)~geohash+bio1+bio2+bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+
 #                       bio13+bio14+bio15+bio16+bio17+bio18+bio19+altdat, data=train, 
 #                     method = "xgbTree", nrounds=150,max_depth=3,eta=0.4,
  #                    gamma=0,colsample_bytree=0.8,
   #                   min_child_weight=1,subsample=1)

#varImp(modelCTBoost)


#plot(modelCTBoost)

#model <- xgb.dump(modelCTBoost, with.stats = T)
#odel[1:10] #This statement prints top 10 nodes of the model

library(DiagrammeR)
xgb.plot.multi.trees(model = modelCTBoost, feature_names = levels(train[,4:23]), features_keep = 15)

# Save and load model for future
#xgb.save(modelCTBoost, "xgboost.model")
#modelCTBoost <- xgb.load("xgboost.model")

# Conclusion: Best model so far




###############################_---------------------Neural Net-------------------------######################
library(neuralnet)
train3 <- train
train3 <- data.frame(scale(train[,4:23]))
train3$occurence <- c(train$occurence)
nn <- neuralnet(as.factor(occurence)~., data=train, hidden=10, stepmax=1e15,
                act.fct="logistic", linear.output = F)
plot(nn)
predictionsNN <- predict(nn, newdata=x_test, type="class")
predictionsNN <- predictionsNN$net.result

predictionsNN <- ifelse(predictionsNN$net.result>0.7,1,0)



###################----------------------------Model Summary Stats/Confusion Matrix-------------############################################
predictionsGLM <- predict(modelGLM, x_test, type="response")
fitted.predsGLM <- ifelse(predictionsGLM > 0.7, 1, 0)
fitted.predsGLM <- as.factor(fitted.predsGLM)
predictionsSVM <- predict(modelSVM, x_test[,3:22], type="prob")
fitted.predsSVM <- ifelse(predictionsSVM[,2] > 0.7, 1, 0)
predictionsCT <- predict(modelCT, x_test_CT[,3:23], type="class")

# Specifically for xgboost
x_test_CT <- x_test
lapply(x_test_CT, class)
predictionsCTBoost <- predict(modelCTBoost, as.matrix(x_test_CT[,4:23]))
predictionsCTBoost_probs <- predictionsCTBoost
predictionsCTBoost <- as.numeric(predictionsCTBoost > 0.5)

predictionsCTBoost_probs


totaldata <- x_test_CT
totaldata$predictionProb <- predictionsCTBoost_probs
totaldata$predictionClass <- predictionsCTBoost

# Get accuracy, false positives, etc stats
library(caret)
library(klaR)
confusionMatrix(fitted.predsGLM, y_test)
confusionMatrix(as.factor(fitted.predsSVM), y_test)
confusionMatrix(predictionsCT, y_test)
confusionMatrix(as.factor(LogResults), y_test)
confusionMatrix(as.factor(Gausresults), y_test)
confusionMatrix(as.factor(predictionsCTBoost), y_test, positive="1")
confusionMatrix(as.factor(predictionsNN), y_test)

accuracy_table <- function(obs, pred){
  data.frame(TP = sum(obs == 1 & pred == 1),
             TN = sum(obs == 0 & pred == 0),
             FP = sum(obs == 0 & pred == 1),
             FN = sum(obs == 1 & pred == 0))
}

accuracy_table(as.factor(predictionsCTBoost), y_test)
######################-----------------------------Final Plotting------------------------------#####################################################

library(ggplot2)
library(maps)
library(ggmap)

USA <- map_data('usa')
g <- ggplot(USA, aes(long, lat)) + 
  geom_polygon(aes(group=group),fill="white",colour="black",size=0.5) +
  coord_equal() + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude') +
  theme_bw()
g <- g + 
  geom_point(data=totaldata,aes(Longitude, Latitude, color=totaldata$predictionProb)) +
  scale_color_gradient(low="dark blue", high="red", name='Probability of Occurence') +
  theme(
    legend.position = 'bottom',
    legend.key.size = unit(1, "cm")
  ) + ggtitle("Tree of Heaven Model with XGBoost") + theme(plot.title = element_text(hjust = 0.5))
g

# Actual Plot
USA <- map_data('usa')
g1 <- ggplot(USA, aes(long, lat)) + 
  geom_polygon(aes(group=group),fill="white",colour="black",size=0.5) +
  coord_equal() + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude') +
  theme_bw()
g1 <- g1 + 
  geom_point(data=totaldata,aes(Longitude, Latitude, color=as.factor(totaldata$occurence))) +
  theme(
    legend.position = 'bottom',
    legend.key.size = unit(1, "cm")
  ) + ggtitle("Tree of Heaven Actual Points") + theme(plot.title = element_text(hjust = 0.5)) + labs(color="Occurence")
g1







# Breakdown our tree with useful plots
#install.packages("devtools") 
library(devtools) 
#install_github("AppliedDataSciencePartners/xgboostExplainer")

col_names = attr(xgmatrix, ".Dimnames")[[2]]
imp = xgb.importance(col_names, modelCTBoost)
xgb.plot.importance(imp)

library(xgboostExplainer)
explainer = buildExplainer(modelCTBoost,xgmatrix, type="binary", base_score = 0.5, trees_idx = NULL)
#test.y <- as.data.frame(test.y)
#sparse_matrix <- sparse.model.matrix(test.y~geohash+bio1+bio2+bio3+bio4+bio5+bio6+bio7+bio8+bio9+bio10+bio11+bio12+
                                       #bio13+bio14+bio15+bio16+bio17+bio18+bio19+altdat, data = x_test_CT)[,-1]
xgmatrix = xgb.DMatrix(data=as.matrix(totaldata[,4:23]), label=as.numeric(as.factor(totaldata$occurence))-1) 
pred.breakdown = explainPredictions(modelCTBoost, explainer, xgmatrix)
cat('Breakdown Complete','\n')
weights = rowSums(pred.breakdown)
pred.xgb = 1/(1+exp(-weights))
cat(max(predictionsCTBoost-pred.xgb),'\n')
idx_to_get = as.integer(802)
x_test_CT[idx_to_get,-"left"]
showWaterfall(modelCTBoost, explainer, xgmatrix, data.matrix(x_test_CT) ,idx_to_get, type = "binary")
####### IMPACT AGAINST VARIABLE VALUE
plot(y_test, pred.breakdown[], cex=0.4, pch=16, xlab = "Satisfaction Level", ylab = "Satisfaction Level impact on log-odds")
plot(test[,last_evaluation], pred.breakdown[,last_evaluation], cex=0.4, pch=16, xlab = "Last evaluation", ylab = "Last evaluation impact on log-odds")
cr <- colorRamp(c("blue", "red"))
plot(test[,last_evaluation], pred.breakdown[,last_evaluation], col = rgb(cr(round(test[,satisfaction_level])), max=255), cex=0.4, pch=16, xlab = "Last evaluation", ylab = "Last evaluation impact on log-odds")






###############################---------------------------R Shiny-----------------------------###############################################
