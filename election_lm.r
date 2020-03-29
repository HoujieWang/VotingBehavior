{
  setwd("/Users/sangchanlee/Downloads/election_combined")
  sang_election = read.csv(file = "election_couty_cleaned.csv")
  load(file = "full_data.Rdata")
}
library(glmnet)
library(caret)

head(full_data[,1:4])
head(sang_election[,1:4])

# removing missing value county 
full_data = full_data[-632,]
sang_election= sang_election[-623,]

m1 = (which(full_data[,3] %in% sang_election[,4]))
data_feature_meatch = full_data[m1,]

m2 = (which(sang_election[,4] %in% full_data[,3]))
data_response_match = sang_election[m2,]
data_response_match = sang_election[-729,]

data_response_match = data.frame(data_response_match[,3:4] ,data_response_match[,59:74])
head(data_response_match)

# log odds 
logOdds_dem2016 = log(data_response_match$pct_dem_16/(1-data_response_match$pct_dem_16))
logOdds_dem2012 = log(data_response_match$pct_dem_12/(1-data_response_match$pct_dem_12))

y = logOdds_dem2016 - logOdds_dem2012

# y = abs(data_response_match$Demvotes16 - data_response_match$Demvotes12)
# y = scale(y)

X = data_feature_meatch[,-1:-4]
X = apply(X, 2, scale)

# set.seed(101)
# cv_lasso = cv.glmnet(X, y)
# lasso_coef = coef(cv_lasso, s = "lambda.min")
# lasso_vs = X[,-which(lasso_coef[-1,1]==0)]


df = data.frame(y, X)
set.seed(101)
model <- train(
  y ~., data = df, method = "glmnet",
  family = "gaussian",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

# Best tuning parameter
model$bestTune
elastic_coef = coef(model$finalModel, model$bestTune$lambda)

?glmnet

# removing all variables which coefficients are zero 
a = which(elastic_coef[-1] == 0)
X = X[,-a]
data = data.frame(y, X)
head(data)
dim(data)


# X_features = data_feature_meatch[,-1:-4]
# par(mfrow = c(2,2))


# data = data.frame(data_response_match[,1:2], y, lasso_vs, check.names = FALSE)


set.seed(101)
indexing <- createDataPartition(y = data$y, p = 0.75,list = FALSE)
data_tr = data[indexing,]
data_tst = data[-indexing,]
data_train = data_tr[,-1:-2]
data_test = data_tst[,-1:-2]
head(data_train)



knn_fit = train(y~., 
                data = data_train, 
                trControl = trainControl(method = 'repeatedcv', number = 10, repeats = 3), 
                method = 'knn', 
                tuneLength = 10
                )
plot(knn_fit)
knn_pred = predict(knn_fit, newdata = data_test)
mean((data_test$y - knn_pred)^2)


rf_fit = train(y~., 
               data = data_train, 
               trControl = trainControl(method = 'repeatedcv', number = 10, repeats = 3), 
               method = 'rf', 
               tuneLength = 5
               )
rf_pred = predict(rf_fit, newdata = data_test)
mean((data_test$y - rf_pred)^2)







