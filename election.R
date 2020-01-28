{
setwd("/Users/sangchanlee/Downloads/Spring 2020/STAT 482/election data")
df = read.csv(file = "CleanedElection.csv")
}

# selecting variables for response variable 
y_matrix = df[,59:74]
y_matrix = y_matrix[,c(-11,-12)]
head(y_matrix)

# democratic = 0 and republican = 1 
for (i in 1:nrow(y_matrix)){
  if(y_matrix$pct_dem_16[i] > y_matrix$pct_gop_16[i])
    y_matrix$y_2016[i] = 0
  else
    y_matrix$y_2016[i] = 1
  
  if(y_matrix$pct_dem_12[i] > y_matrix$pct_gop_12[i])
    y_matrix$y_2012[i] = 0
  else
    y_matrix$y_2012[i] = 1
}

# excluding variables for response 
X_matrix = df[,c(-59:-74)]

# excluding first five variables(lon, lat, state, county_nam)
X_matrix = X_matrix[,c(-1:-5)]

# excluding AFFGEOID_1 since it is combinations of numbers and characters. The explanatin does not exist in the website. 
X_matrix = X_matrix[,-52]

# make a data frame for response in 2012 and 2016 
response_matrix = data.frame(y_matrix$y_2012, y_matrix$y_2016)
colnames(response_matrix) = c('y_2012','y_2016')


# ic.glmnet, cv.glmnet 
# variable selection using cv.glmnet 
# type.measure = "class" applies to binomial and multinomial logistic regression only, and gives misclassification error.
fit2012 = cv.glmnet(x = as.matrix(X_matrix), y = as.factor(response_matrix$y_2012), family = "binomial", type.measure = "class")
fit2016 = cv.glmnet(x = as.matrix(X_matrix), y = as.factor(response_matrix$y_2016), family = "binomial", type.measure = "class")
result1 = coef(fit2012, s = "lambda.min")
result2 = coef(fit2016, s = "lambda.min")

cbind(result1, result2)


# To find counties which changed their potilical response from 2012 to 2016. 0 represents no change and 1 represents change. 
for (i in 1:nrow(response_matrix)){
  if (response_matrix[i,1] - response_matrix[i,2] == 0){
    response_matrix$change[i] = 0
  }
  else 
    response_matrix$change[i] = 1
}
head(response_matrix)














