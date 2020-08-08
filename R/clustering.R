source("C:/virtual_learning/R/global.R")

# get the numerics and the id var
nums <- names(resp_wide1)[unlist(lapply(resp_wide1, is.numeric))]

cdf0 <- resp_wide1 %>% 
  select(RECORD_ID, all_of(nums)) %>% 
  select(-starts_with("DISTRICT"))

# some of these will have missing values. This is not good. 
cdf_cc0 <- cdf0 %>% 
  filter(complete.cases(.))

cdf_counts <- data.frame(col_sum = colSums(cdf_cc0[, -1])) %>% 
  rownames_to_column() %>% 
  as_tibble()

use_cols <- cdf_counts %>% 
  filter(col_sum > 1) %>% 
  pull(rowname)

cdf_cc1 <- cdf_cc0 %>% 
  select(all_of(use_cols))

cluster_vars <- scale(cdf_cc1)

set.seed(42)

# cboot <- clusterboot(cluster_vars, clustermethod = kmeansCBI,
#                      runs = 100, iter.max = 100, krange = 10,
#                       seed = 42)
# 
# 
# clustering_ch <- kmeansruns(cluster_vars, krange = 1:10, criterion = "asw")
# clustering_ch$cluster
# clustering_ch$bestk
# clustering_ch$crit
# 
# cboot$result$result$cluster
# cboot$bootmean

# Run clustering for 1 to 10
ks <- 2:15
set.seed(42)
clust_list0 <- lapply(ks, function(m){
  clust_out <- kmeans(cluster_vars, centers = m, nstart = 500)
  clust_assign <- clust_out$cluster
  matching <- clust_out$betweenss / clust_out$totss
  list(clust_assign = clust_assign, 
       matching = matching)
  clust_assign
})

clust_list0

names_clust_list <- paste0("Cluster-", ks)
names(clust_list0) <- names_clust_list

cluster_df <- bind_cols(clust_list0)

cdf_cc1 <- cdf_cc0 %>% 
  bind_cols(cluster_df) 

# get reference information
rec_clust <- cdf_cc1 %>% 
  select(RECORD_ID, contains("Cluster-"))

resp1 <- resp0 %>% 
  left_join(rec_clust)

write_sheet(resp1, "https://docs.google.com/spreadsheets/d/1lqTpN0id5B30J5Tg7xDvo-DPXjC42THhsRDMPD4hFY0/edit#gid=0",
            sheet = "CLUSTER_OUT")

