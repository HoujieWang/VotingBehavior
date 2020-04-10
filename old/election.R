library(glmnet); library(spdep); library(stats); library(cluster)

df = read.csv(file = "CleanedElection.csv")
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

df.2012 = df[, as.vector(result1 != 0)]; df.2016 = df[, as.vector(result2 != 0)]
# To find counties which changed their potilical response from 2012 to 2016. 0 represents no change and 1 represents change. 
for (i in 1:nrow(response_matrix)){
  if (response_matrix[i,1] - response_matrix[i,2] == 0){
    response_matrix$change[i] = 0
  }
  else 
    response_matrix$change[i] = 1
}
head(response_matrix)

Kmeans.df = kmeans(df[, 1:2], centers = 2)


bh <- st_read(system.file("etc/shapes/bhicv.shp",
                          package="spdep")[1], quiet=TRUE)
st_crs(bh) <- "+proj=longlat +ellps=WGS84"
### data standardized
dpad <- data.frame(scale(as.data.frame(bh)[,5:8]))
bh.nb <- poly2nb(bh)
### calculating costs
lcosts <- nbcosts(bh.nb, dpad)
### making listw
nb.w <- nb2listw(bh.nb, lcosts, style="B")
### find a minimum spanning tree
mst.bh <- mstree(nb.w,5)
### the mstree plot
par(mar=c(0,0,0,0))
plot(st_geometry(bh), border=gray(.5))
plot(mst.bh, coordinates(as(bh, "Spatial")), col=2,
     cex.lab=.6, cex.circles=0.035, fg="blue", add=TRUE)
### three groups with no restriction
res1 <- skater(mst.bh[,1:2], dpad, 2)
### groups size
table(res1$groups)
### the skater plot
opar <- par(mar=c(0,0,0,0))
plot(res1, coordinates(as(bh, "Spatial")), cex.circles=0.035, cex.lab=.7)
### the skater plot, using other colors
plot(res1, coordinates(as(bh, "Spatial")), cex.circles=0.035, cex.lab=.7,
     groups.colors=heat.colors(length(res1$ed)))
### the Spatial Polygons plot
plot(st_geometry(bh), col=heat.colors(length(res1$edg))[res1$groups])
par(opar)
### EXPERT OPTIONS
### more one partition
res1b <- skater(res1, dpad, 1)
### length groups frequency
table(res1$groups)
table(res1b$groups)
### thee groups with minimum population
res2 <- skater(mst.bh[,1:2], dpad, 2, 200000, bh$Pop)
table(res2$groups)
### thee groups with minimun number of areas
res3 <- skater(mst.bh[,1:2], dpad, 2, 3, rep(1,nrow(bh)))
table(res3$groups)
### thee groups with minimun and maximun number of areas
res4 <- skater(mst.bh[,1:2], dpad, 2, c(20,50), rep(1,nrow(bh)))
table(res4$groups)
### if I want to get groups with 20 to 40 elements
res5 <- skater(mst.bh[,1:2], dpad, 2,
               c(20,40), rep(1,nrow(bh))) ## DON'T MAKE DIVISIONS
table(res5$groups)
### In this MST don't have groups with this restrictions
### In this case, first I do one division
### with the minimun criteria
res5a <- skater(mst.bh[,1:2], dpad, 1, 20, rep(1,nrow(bh)))
table(res5a$groups)
126
skater
### and do more one division with the full criteria
res5b <- skater(res5a, dpad, 1, c(20, 40), rep(1,nrow(bh)))
table(res5b$groups)
### and do more one division with the full criteria
res5c <- skater(res5b, dpad, 1, c(20, 40), rep(1,nrow(bh)))
table(res5c$groups)
### It don't have another divison with this criteria
res5d <- skater(res5c, dpad, 1, c(20, 40), rep(1,nrow(bh)))
table(res5d$groups)
## Not run:
data(boston, package="spData")
bh.nb <- boston.soi
dpad <- data.frame(scale(boston.c[,c(7:10)]))
### calculating costs
system.time(lcosts <- nbcosts(bh.nb, dpad))
### making listw
nb.w <- nb2listw(bh.nb, lcosts, style="B")
### find a minimum spanning tree
mst.bh <- mstree(nb.w,5)
### three groups with no restriction
system.time(res1 <- skater(mst.bh[,1:2], dpad, 2))
library(parallel)
nc <- detectCores(logical=FALSE)
# set nc to 1L here
if (nc > 1L) nc <- 1L
coresOpt <- get.coresOption()
invisible(set.coresOption(nc))
if(!get.mcOption()) {
  # no-op, "snow" parallel calculation not available
  cl <- makeCluster(get.coresOption())
  set.ClusterOption(cl)
}
### calculating costs
system.time(plcosts <- nbcosts(bh.nb, dpad))
all.equal(lcosts, plcosts, check.attributes=FALSE)
### making listw
pnb.w <- nb2listw(bh.nb, plcosts, style="B")
### find a minimum spanning tree
pmst.bh <- mstree(pnb.w,5)
### three groups with no restriction
system.time(pres1 <- skater(pmst.bh[,1:2], dpad, 2))
if(!get.mcOption()) {
  set.ClusterOption(NULL)
  stopCluster(cl)
}
all.equal(res1, pres1, check.attributes=FALSE)
invisible(set.coresOption(coresOpt))
## End(Not run)









