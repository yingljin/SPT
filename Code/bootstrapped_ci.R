library(tidyverse)
library(dbscan) #for fast nearest-neigbor search using kd-trees

clean_data <- readRDS("Data/example_data.rds")

# one lesion from one subject scan at four times
# for every voxel, there is a QSM and a MWF
# for every lesions at every time point, there is a single correlation




# create a function with the following arguments
# les_id = a specified lesion id
# prop = proportion of data to sample 
# k_neighbors = number of nearest neigbors
# iterations = number of bootstrapped samples 
# NOTE: prop and k are choosen such that prop*k equals to one to ensure similar sample sizes 
neighbor_correlation <- function(les_id, prop, k_neighbors, iterations) {
  
  # filter data for specified lesion
  data <- clean_data %>% filter(lesion_id == les_id) 
  
  # identify the unique voxels (using x,y,z coordinates)
  unique_voxels <- unique(data[8:10])
  
  # find k nearest neighbors for EACH unique voxel using kNN function from dbscan()
  nn <- kNN(x = unique_voxels, k = k_neighbors) 
  
  # neighbor_data contains the nearest k neighbor for ALL points
  neighbor_data <- nn$id
  
  total_rows <- nrow(neighbor_data)
  
  # specify the # of rows to sample
  sample_size <- round(total_rows * prop)
  
  # create a function for bootstrap resampling; sampling WITH replacement 
  bootstrap_function <- function() {
    
    # generate random row indices with replacement
    random_indices <- sample(1:total_rows, size = sample_size, replace = TRUE)
    
    # use the random indices to extract the sampled rows
    sampled_rows <- neighbor_data[random_indices, ]
    
    # include data across ALL time points for the chosen voxel!! 
    sampled_data <- unique_voxels[c(sampled_rows),]
    
    # use right_join() to pull the relevant data for the sampled voxels!
    correlation_data <- data %>%
      right_join(sampled_data, by = c("x", "y", "z"),
                 relationship = "many-to-many")
    
    cor_res <- cor.test(correlation_data$MWF, correlation_data$QSM, method = "pearson")
    
    # Return the correlation results 
    return(cor_res)
  } #END OF bootstrap_function()
  
  # generate bootstrap samples
  bootstrap_samples <- map(1:iterations, ~bootstrap_function())
  
  return(bootstrap_samples)
} #END OF FUNCTION

# pink a lesion
les_id <- "0791_20200124_27"
prop <- 0.1
k_neighbors <- 10

# create 1000 bootstrapped samples for the selected lesion
final_res <- neighbor_correlation(les_id, prop=0.1, k_neighbors = 10, 1000) 

# extract the 1000 correlation estimates
correlations <- map_df(final_res, ~.x$estimate)

# plot distribution of the correlations 
ggplot(correlations) +
  geom_histogram(aes(x=cor), color = "white", fill = "salmon") +
  labs(x = "correlation", title = les_id) +
  theme_bw()

# calculate the 95% CI 
lower_bound <- quantile(correlations$cor, 0.025) 
upper_bound <- quantile(correlations$cor, 0.975)
cat("95% Bootstrap CI: (",lower_bound, ",", upper_bound,")\n")

