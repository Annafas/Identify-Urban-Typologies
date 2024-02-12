# Load necessary libraries (if not already installed)
install.packages("corrr")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("readxl")
install.packages("NbClust")
install.packages("cluster")

library(corrr)
library(ggplot2)
library(ggcorrplot)
library(FactoMineR)
library (factoextra)
library(readxl)
library (NbClust)
library(corrplot)
library(cluster)

# Load your data from an Excel file (Put the path where your data is located)
network_data <- read_excel("C:\\network.xls")

#keep only the data needed for PCA
network_data2 <- network_data[,4:21]

#Structure of data 
str (network_data2)
summary(network_data2)

#Check for null values
colSums(is.na(network_data2))

#Normalizing the data (in order to create new data with only numeric columns)
numerical_data <- network_data[,4:21]
head(numerical_data)

#Standardize the data
data_normalized <- scale(numerical_data)
head(data_normalized)

#Compute the correlation matrix
corr_matrix <- cor(data_normalized)
p <- ggcorrplot(corr_matrix)
p+scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "#E46726", mid = "white", midpoint = 0.5)

#Run PCA
network_pca <- prcomp (data_normalized, center = TRUE, scale. = TRUE)
summary(network_pca)

#Selection of principal components
#Scree Plot of Variance (elbow method)
  fviz_eig(network_pca,
           addlabels = TRUE,
           ncp = 18,
           ylim = c(0, 80))

#Scree plot for eigenvalues (Kaiser's method)
fviz_eig(network_pca, 
         addlabels = TRUE,
         ncp = 18,
         choice="eigenvalue") +
  geom_hline(yintercept=1, 
             linetype="dashed", 
             color = "red")

# Biplot of the Attributes
fviz_pca_biplot(network_pca,
                col.var = "#2E9FDF", #Variables color
                col.ind = "#ccc8c8") ## Individuals color

#Cos2
var <- get_pca_var(network_pca)
print(var)
head(var$cos2)

fviz_cos2(network_pca, choice = "var", axes = 1:3)

#Biplot combined with cos2 
fviz_pca_var(network_pca,
             col.var = "cos2", # Color by the quality of representation
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE) #Avoid text overlapping

#Extract the component values (scores)
pca_scores <- network_pca$x[, 1:3]


# Create a data frame with the PCA components and the original ID
pca_data <- data.frame(ID = network_data$id, PC1 = pca_scores[, 1], PC2 = pca_scores[, 2], PC3 = pca_scores[, 3])

# Save to CSV (Put the path where you want the file to be saved)
write.csv(pca_data, file = "C:\\pca_results.csv", row.names = FALSE)

#Methods for determining the optimal number of clusters
#via silhouette method
fviz_nbclust(pca_scores, cluster::pam, method = "silhouette")+labs(subtitle = "Silhouette method")

#via elbow method
fviz_nbclust(pca_scores,cluster::clara, method = "wss") + labs(subtitle = "Elbow method")

#via Gap statistic method
# nboot = 50 to keep the function speedy
set.seed(123)
fviz_nbclust(pca_scores,cluster::pam, nstart = 25,  method = "gap_stat", nboot = 50)+
labs(subtitle = "Gap statistic method")

#Clustering k-medoids
# Install and load necessary packages
install.packages(c("fpc", "leaflet"))
install.packages("sf")

# Perform k-medoids clustering 
#make this example reproducible
set.seed(1)

#perform k-medoids clustering with your desired number of clusters
kmed <- pam(pca_scores, k = 5)

#view results
kmed

#add cluster assignment to original data
final_data <- cbind(pca_data, cluster = kmed$cluster)

#view final data
head(final_data)

# Save to CSV (Put the path where you want the file to be saved)
write.csv(final_data, file = "C:\\network_clusters5.csv", row.names = FALSE)

