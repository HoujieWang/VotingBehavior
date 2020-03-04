{
  setwd("/Users/sangchanlee/Downloads/election_combined")
  sang_election = read.csv(file = "election_couty_cleaned.csv")
  load(file = "full_data.Rdata")
}


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

y = abs(data_response_match$Demvotes16 - data_response_match$Demvotes12)
y = scale(y)

X = data_feature_meatch[,-1:-4]
X = apply(X, 2, scale)

cv_lasso = cv.glmnet(X, y)
lasso_coef = coef(cv_lasso, s = "lambda.min")

lasso_vs = X[,-which(lasso_coef[-1,1]==0)]

X_features = data_feature_meatch[,-1:-4]
par(mfrow = c(2,2))


X_features = as.matrix(X_features)
fit_lm = lm(y~X_features)
plot(fit_lm)


# generalized log transforamtion 
# z <- 1
# x <- selected.data_3D_exp[,-1]
# glog_3Dexp <- log2((x+sqrt((x^2)+(z^2)))/2)



df = insert(a, 4, "State")
df = df[-65]
colnames(full_data) = df



write.csv(lasso_coef, file = "lasso_coef.csv")
install.packages("EnvStats")
library(EnvStats)




