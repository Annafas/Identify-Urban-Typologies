# Load necessary libraries (if not already installed)
install.packages("readxl")
install.packages("cluster") # for silhouette and other clustering metrics
install.packages("factoextra")

library(readxl)
library(cluster) 
library (factoextra)

# Load your data from an Excel file (Put the path where your data is located)
density_data <- read_excel("C:\\build_density.xlsx")

#keep only the data needed for PCA
density_data2 <- density_data[,2:3]

#Structure of data 
str (density_data2)
summary(density_data2)

#Check for null values
colSums(is.na(density_data2))

#Normalizing the data (in order to create new data with only numeric columns)
numerical_data <- density_data[,2:3]
head(numerical_data)

#Standardize the data
data_normalized <- scale(numerical_data)
head(data_normalized)

#Methods for determining the optimal number of clusters

fviz_nbclust(data_normalized, kmeans, method='silhouette')

fviz_nbclust(data_normalized,kmeans, method = "wss") + labs(subtitle = "Elbow method")

set.seed(123)
fviz_nbclust(data_normalized,kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method") 

#Clustering k-means 
#perform k-means clustering with your desired number of clusters
k <- 8 
kmeans_model <- kmeans(data_normalized, centers = k, nstart = 20)

# Print the cluster centers
print(kmeans_model$centers)

# Assign cluster labels to each data point
cluster_labels <- kmeans_model$cluster

# Attach cluster labels to the original dataset
density_clustered <- cbind(data_normalized, Cluster = cluster_labels)

# View the resulting dataset with cluster assignments
print(head(density_clustered))

# Create a data frame with the clusters and the original ID
final_data <- data.frame(ID = density_data$Block_ID, data_normalized, Cluster = cluster_labels)

# Save to CSV (Put the path where you want the file to be saved)
write.csv(final_data, file = "C:\\build_density_clusters7.csv", row.names = FALSE)

