df<-ADS_for_Clustering
# Select relevant columns for clustering
selected_columns <- c('Store#', 'Total_Sales', 'Tire_Quantity', 'Gross_Profit', 'Total_Discount',
                      'COVID_Sales', 'Total_Transactions', 'Customer_Count', 'Gross_Profit_Percent',
                      'Sales_BF GOODRICH', 'Sales_FALKEN', 'Sales_GOODYEAR', 'Sales_KELLY',
                      'Sales_MICHELIN', 'Sales_MULTI-MILE', 'Sales_SOLAR', 'Sales_SUMITOMO',
                      'Sales_VANDERBILT', 'Sales_VREDESTEIN')
# Perform feature scaling on selected columns
scaled_data <- scale(df[selected_columns])

# Perform K-means clustering
k <- 4  # number of clusters
kmeans_result <- kmeans(scaled_data, centers = k, nstart = 10)

# Add the cluster labels to the data frame
df$cluster <- kmeans_result$cluster
# Print the resulting clusters
table(df$cluster)

# Plot the cluster visualization
plot(df$`Store#`, df$Total_Sales, col = df$cluster, pch = 16, xlab = 'Store', ylab = 'Total Sales',
     main = 'K-means Clustering')

# Add a color legend
legend('topleft', legend = paste('Cluster', 1:k), col = 1:k, pch = 16)
##########################################################################################################



library(dplyr)
df <- select(df,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))

#WSS to choose the max number

wssplot <- function(df, nc=15, seed=1234){
  wss <- (nrow(df)-1)*sum(apply(df,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(df, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(df)

#Kmeans cluster
CM= kmeans(df,4)

#evaluating custer plot
install.packages("ggfortify")
library(ggfortify)
autoplot(CM,df,frame=TRUE)

#cluster centre
CM$centers