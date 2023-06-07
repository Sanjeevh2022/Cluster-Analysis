#Step 1 Load the dataset and libraries needed
oes <-readRDS(file.choose()) # load oes.rds

summary(oes) # quickly review summary
library(dendextend)
library(readr)
library(psych)
library(dplyr)
library(ggplot2)
library(tibble)
library(cluster)
library(tidyr)
library(purrr)
install.packages("ggdendro")
library(ggdendro)

# Step 2 Calculate Euclidean distance between customers
# Calculate Euclidean distance between the occupations
dist_oes <- dist(oes, method = 'euclidean')

# Step 3 Generate a average linkage analysis
hc_oes <- hclust(dist_oes, method = 'average')

# Create a dendrogram object from the hclust variable
dend_oes <- as.dendrogram(hc_oes)

# Step 4 Plot the dendrogram
# Plot the dendrogram
plot(dend_oes)
# Use rownames_to_column to move the rownames into a column of the data frame
df_oes <- rownames_to_column(as.data.frame(oes), var = 'occupation')


# Step 5 Create a cluster assignment vector at h = 100000
cut_oes <- cutree(hc_oes, h = 100000)


# Step 6 Generate the segmented customers dataframe
clust_oes <- mutate(df_oes, cluster = cut_oes)


# Step 7 Lets make things beautiful – create a clean dataframe
# Create a tidy data frame by gathering the year and values into two columns
gathered_oes <- gather(data = clust_oes,
                       key = year,
                       value = mean_salary,
                       -occupation, -cluster)

# Step 8 sort the clustering assignments
# View the clustering assignments by sorting the cluster assignment vector
sort(cut_oes)
# Plot the relationship between mean_salary and year and color the lines by
#the assigned cluster
ggplot(gathered_oes, aes(x = year, y = mean_salary, color = factor(cluster))) +
  geom_line(aes(group = occupation))
