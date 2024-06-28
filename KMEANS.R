setwd("C:/Users/james.zhou1/Downloads")
library(data.table)

# read Airbnb dataset
bnb = fread("Airbnb_Data.csv")

# exponentiate (unlog) price column for easier interpretation
bnb$log_price = exp(bnb$log_price)
names(bnb)[names(bnb) == "log_price"] = 'price'


# filter to numerical values only since k-means can only be done with numeric variables
numerical = Filter(is.numeric, bnb)
# remove id column since this is an arbitrary value
numerical = numerical[,-1] 

# remove observations with NA values
# although these may be useful observations, setting them to 0 could skew the
# results and our dataset is large enough
numerical = na.omit(numerical) 

# scale the dataset since the variables have very different scales
scaled_data = scale(numerical)

# k-means clustering with k=2, 3, 4
set.seed(1)
km2 = kmeans(scaled_data, 2, nstart=10)
set.seed(1)
km3 = kmeans(scaled_data, 3, nstart=10)
set.seed(1)
km4 = kmeans(scaled_data, 4, nstart=10)

# plots (take a while to run)
plot(numerical, col = (km2$cluster), main="K-means Clustering, k=2", pch=19, cex=1)
plot(numerical, col = (km3$cluster), main="K-means Clustering, k=3", pch=19, cex=1)
plot(numerical, col = (km4$cluster), main="K-means Clustering, k=4", pch=19, cex=1)

