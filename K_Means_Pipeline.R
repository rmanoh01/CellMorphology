
# Fresh start - load libraries first
library(MicrogliaMorphologyR)
library(plotly)
library(dplyr)
library(cluster)
library(NbClust)
library(factoextra)
library(ggplot2)
library(tidyr)
library(broom)


# UPDATE THE VALUES BELOW WITH THE PROPER FILEPATHS


fraclac.dir <- 
skeleton.dir <- 

output_dir <- 


# Define paths to corrected or additional data (if you are merging in multiple
# pipeline processing batches into one R analysis. If you are not, go down to
# the 'MERGE IN CORRECTED DATA OR ADDITIONAL ANALYSIS DATA' below and comment
# it out!
fraclac.dir.corrected <- 
skeleton.dir.corrected <- 



fraclac <- fraclac_tidying(fraclac.dir) 
skeleton <- skeleton_tidying(skeleton.dir)
data <- merge_data(fraclac, skeleton)
head(data)


# ============================================================
# FILTERING SECTION - Edit this to exclude images, entire animals
# ============================================================
# Define Mouse_IDs to exclude (edit this list as needed)
exclude_mouse_ids <- c()  # Add more IDs like: c("09292025004", etc)

# Define specific images to exclude (edit this list as needed)
exclude_images <- c()  # Add more like: c("processed_05122025002_Transition_1_B", "image2", "image3")

# Extract metadata FIRST so we have the Mouse_ID column
# Update if your filenames have a different structure. This line below works 
# for example filename: processed_05122025002_Transition_1_B
finaldata <- metadata_columns(data, c("Processed","Mouse_ID", "Brain_Region", "Image_Number_In_Region", "Treatment"), sep="_")

# Apply filter to remove matching Mouse_IDs
print(paste("Total rows before filtering:", nrow(finaldata)))

finaldata_filtered <- finaldata %>%
  filter(!Mouse_ID %in% exclude_mouse_ids) %>%
  filter(!grepl(paste(exclude_images, collapse="|"), UniqueID))

print(paste("Total rows after filtering:", nrow(finaldata_filtered)))
print(paste("Rows removed:", nrow(finaldata) - nrow(finaldata_filtered)))

# Continue with filtered data
finaldata <- finaldata_filtered
# ============================================================



# ============================================================
# MERGE IN CORRECTED DATA OR ADDITIONAL ANALYSIS DATA
# ============================================================
print("Loading corrected or additional data...")


# Load and process corrected data
fraclac.corrected <- fraclac_tidying(fraclac.dir.corrected) 
skeleton.corrected <- skeleton_tidying(skeleton.dir.corrected)
data.corrected <- merge_data(fraclac.corrected, skeleton.corrected)

# Extract metadata for corrected data
data.corrected.meta <- metadata_columns(data.corrected, 
                                        c("Processed","Mouse_ID", "Brain_Region", "Image_Number_In_Region", "Treatment"), 
                                        sep="_")

# Optional: Filter corrected data for specific images if needed
data.corrected.meta <- data.corrected.meta %>%
  filter(!grepl(paste(exclude_images, collapse="|"), UniqueID))

print(paste("Corrected data rows:", nrow(data.corrected.meta)))

# Merge corrected data with filtered data
finaldata <- bind_rows(finaldata, data.corrected.meta)

print(paste("Total rows after merging corrected or additional data:", nrow(finaldata)))
# ============================================================





# ============================================================
# NEW: FILTER FOR CELLS WITH 1+ BRANCHES
# ============================================================
print(paste("Total cells before branch filtering:", nrow(finaldata)))

finaldata <- finaldata %>%
  filter(`# of branches` >= 1)

print(paste("Total cells after branch filtering:", nrow(finaldata)))
print(paste("Cells with 0 branches removed:", nrow(finaldata_filtered) - nrow(finaldata)))

# ============================================================



# ============================================================
# NEW: FILTER OUT HIGHLY CIRCULAR CELLS (NOISE)
# I recommend messing around with these settings quite a bit 
# based on your data. You want to filter out noise but not 
# real signal based on your data, which is hard. The 
# ColorByCluster visualization will help you see this 
# ============================================================
print(paste("Total cells before circularity filtering:", nrow(finaldata)))

# Optional: Check circularity distribution before filtering
print(summary(finaldata$Circularity))
hist(finaldata$Circularity)

finaldata <- finaldata %>%
  filter(Circularity < 0.98) %>% 
  filter(Area < 20000)  # Remove extreme outliers (less than top ~1%). Please change this if and as needed

print(paste("Total cells after circularity filtering:", nrow(finaldata)))
print(paste("Highly circular cells removed (>= 0.98):", 
            nrow(finaldata_filtered) - nrow(finaldata)))

# ============================================================


# Add this right after your filtering section, before any analysis

# Check current unique values
print("Brain regions BEFORE standardization:")
print(unique(finaldata$Brain_Region))

# Standardize to Title Case (or you can use tolower() for all lowercase)
finaldata <- finaldata %>%
  mutate(
    Brain_Region = str_to_title(Brain_Region)
    #Treatment = str_to_title(Treatment)  # Also standardize treatment if needed
  ) %>%
  # Fix typo: "Tramsition" -> "Transition"
  mutate(
    Brain_Region = case_when(
      Brain_Region == "Tramsition" ~ "Transition",
      TRUE ~ Brain_Region
    )
  )

# Alternative: Use lowercase for everything
# finaldata <- finaldata %>%
#   mutate(Brain_Region = tolower(Brain_Region))

print("Brain regions AFTER standardization:")
print(unique(finaldata$Brain_Region))


# ============================================================
# CREATE (PROXY) BRANCHING AND RAMIFICATION INDEX
# ============================================================


# --- Step 1: Calculate the simple Ramification Index ---
# This creates the new 'ramification_index' column.
finaldata <- finaldata %>%
  mutate(ramification_index = Perimeter / Area)

# 1. DEFINE THE REFINED FEATURE SETS
# Features for the "Complexity Score"
complexity_features <- c(
  "# of branches",
  "# of junctions",
  "# of end point voxels",
  "# of triple points",
  "Relative variation (CV) in radii from circle's center of mass" # Adds shape irregularity
  #"Average branch length" #Remove for microglia, keep for astrocytes
)

# Feature for the "Territory Score"
territory_feature <- "Maximum radius from hull's center of mass"


# --- Create temporary, scaled versions of the features for the calculation ---
scaled_complexity_data <- finaldata %>%
  select(all_of(complexity_features)) %>%
  mutate(across(everything(), ~ as.vector(scale(.))))

scaled_territory_data <- finaldata %>%
  select(all_of(territory_feature)) %>%
  mutate(across(everything(), ~ as.vector(scale(.))))

# --- Calculate the scores from the temporary scaled data ---
complexity_score <- rowSums(scaled_complexity_data)
territory_score <- scaled_territory_data[[1]] # Extract as a vector

# --- Add the scores to the main data frame and calculate the final Proxy_BI ---
finaldata_with_indices <- finaldata %>%
  mutate(
    complexity_score = complexity_score,
    territory_score = territory_score
  ) %>%
  # Normalize scores to be positive and comparable (0-1 range)
  mutate(
    complexity_score_norm = (complexity_score - min(complexity_score)) / (max(complexity_score) - min(complexity_score)),
    territory_score_norm = (territory_score - min(territory_score)) / (max(territory_score) - min(territory_score))
  ) %>%
  # Finally, calculate the Proxy Branching Index
  mutate(Proxy_BI = complexity_score_norm * territory_score_norm) %>%
  # Optional: You can remove the intermediate score columns if you only want the final indices
  select(-complexity_score, -territory_score, -complexity_score_norm, -territory_score_norm) 


# --- VERIFICATION ---
# View the new indices
select(finaldata_with_indices, ID, ramification_index) %>% head()
# ============================================================


# Transform data
print(paste("Total rows before cleaning:", nrow(finaldata_with_indices)))

finaldata_clean <- finaldata_with_indices %>%
  filter(if_all(8:35, ~ !is.na(.) & !is.infinite(.)))

print(paste("Total rows after cleaning:", nrow(finaldata_clean)))
print(paste("Rows removed:", nrow(finaldata_with_indices) - nrow(finaldata_clean)))

# Now transform the cleaned data
finaldata_with_indices_logtransformed <- transform_log(finaldata_clean, 0.1, start=8, end=36)

# Verify it worked
#print("After transformation - Any Inf?", any(is.infinite(as.matrix(finaldata_with_indices_logtransformed[, 7:34]))))
#print("After transformation - Any NA?", any(is.na(finaldata_with_indices_logtransformed[, 7:34])))

# Now PCA should work
pca_data <- pcadata(finaldata_with_indices_logtransformed, featurestart=8, featureend=36, pc.start=1, pc.end=5)


# gather your numerical morphology data into one column ('measure') which contains the feature name, and another column ('value') which contains measured values
finaldata_gathered <- finaldata_clean %>% gather(measure, value, 8:ncol(finaldata))
normalize_logplots(finaldata_gathered,0.1)
#normalize_scaled(finaldata_gathered)
#normalize_minmax(finaldata_gathered)

# Correct the PCA step by using the correct argument names
pca_data <- pcadata(finaldata_with_indices_logtransformed, featurestart=8, featureend=36, pc.start=1, pc.end=5)


# K-means
kmeans_input <- pca_data[, 1:5]
# Optional visualization

pcadata_elbow(finaldata_with_indices_logtransformed, featurestart=8, featureend=36)

fviz_nbclust(kmeans_input, kmeans, method = 'wss', nstart=25, iter.max=100)
fviz_nbclust(kmeans_input, kmeans, method = 'silhouette', nstart=25, iter.max=100)

# Clustering
data_kmeans <- kmeans(kmeans_input, centers=3, nstart=25, iter.max=100)

# Combine results
pca_kmeans <- cbind(pca_data[1:2], finaldata_with_indices_logtransformed, as.data.frame(data_kmeans$cluster)) %>%
  rename(Cluster=`data_kmeans$cluster`)

# Visualize

clusterplots(pca_kmeans, "PC1", "PC2")
clusterfeatures(pca_kmeans, featurestart=10, featureend=38)

pcfeaturecorrelations(pca_data, pc.start=1, pc.end=5, 
                      feature.start=13, feature.end=41, 
                      rthresh=0.75, pthresh=0.05, 
                      title="Correlation between PCs and features")


# Get unique images
unique_images <- pca_kmeans %>%
  distinct(Processed, Mouse_ID, Brain_Region, Image_Number_In_Region, Treatment)
print(unique_images)

# Output directory
output_dir_colorbycluster <- paste0(output_dir, "ColorByCluster_Files/")
if(!dir.exists(output_dir_colorbycluster)) {
  dir.create(output_dir_colorbycluster, recursive = TRUE)
}

for(i in 1:nrow(unique_images)) {
  img <- unique_images[i,]
  
  colorbycluster <- pca_kmeans %>%
    filter(Processed == img$Processed,
           Mouse_ID == img$Mouse_ID,
           Brain_Region == img$Brain_Region,
           Image_Number_In_Region == img$Image_Number_In_Region,
           Treatment == img$Treatment) %>%
    select(Cluster, ID)
  
  filename <- paste0(output_dir_colorbycluster, 
                     img$Processed, "_", 
                     img$Mouse_ID, "_",
                     img$Brain_Region, "_",
                     img$Image_Number_In_Region,"_",
                     img$Treatment, "_data.csv")
  
  write.csv(colorbycluster, filename, row.names=FALSE)
  print(paste("Created:", filename, "with", nrow(colorbycluster), "cells"))
}
# ============================================================
# K-MEANS CLUSTER ANALYSIS BY BRAIN REGION AND TREATMENT
# ============================================================

library(ggplot2)

# Summary statistics for each k-means cluster
cluster_summary_kmeans <- pca_kmeans %>%
  group_by(Cluster) %>%
  summarise(
    n_cells = n(),
    percent = round(n()/nrow(pca_kmeans)*100, 1),
    
    # Complexity metrics
    mean_proxy_bi = mean(Proxy_BI, na.rm = TRUE),
    sd_proxy_bi = sd(Proxy_BI, na.rm = TRUE),
    mean_branches = mean(`# of branches`, na.rm = TRUE),
    sd_branches = sd(`# of branches`, na.rm = TRUE),
    mean_endpoints = mean(`# of end point voxels`, na.rm = TRUE),
    sd_endpoints = sd(`# of end point voxels`, na.rm = TRUE),
    
    # Size/shape metrics
    mean_area = mean(Area, na.rm = TRUE),
    sd_area = sd(Area, na.rm = TRUE),
    mean_circularity = mean(Circularity, na.rm = TRUE),
    sd_circularity = sd(Circularity, na.rm = TRUE),
    
    # Process characteristics
    mean_branch_length = mean(`Average branch length`, na.rm = TRUE),
    sd_branch_length = sd(`Average branch length`, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(Cluster)

print("=== K-MEANS CLUSTER SUMMARY ===")
print(cluster_summary_kmeans)

write.csv(cluster_summary_kmeans, 
          paste0(output_dir, "kmeans_cluster_summary.csv"), 
          row.names = FALSE)

# ============================================================
# SAMPLE SIZE ANALYSIS (CRITICAL FOR INTERPRETATION)
# ============================================================

# Total cells per treatment (overall)
total_by_treatment <- pca_kmeans %>%
  count(Treatment) %>%
  rename(total_cells = n)

print("=== TOTAL CELLS BY TREATMENT ===")
print(total_by_treatment)

# Total cells per region
total_by_region <- pca_kmeans %>%
  count(Brain_Region) %>%
  rename(total_cells = n)

print("=== TOTAL CELLS BY BRAIN REGION ===")
print(total_by_region)

# Total cells per region AND treatment
total_by_region_treatment <- pca_kmeans %>%
  count(Brain_Region, Treatment) %>%
  rename(total_cells = n)

print("=== TOTAL CELLS BY BRAIN REGION AND TREATMENT ===")
print(total_by_region_treatment)

write.csv(total_by_region_treatment, 
          paste0(output_dir, "total_cells_by_region_treatment.csv"), 
          row.names = FALSE)





# ============================================================
# MEANS AND MEDIANS OF PROXY_BI COMPOSITE FEATURES
# ============================================================
# Calculate mean and median for each feature by cluster
cluster_comparison <- pca_kmeans %>%
  group_by(Cluster) %>%
  summarise(
    # Proxy_BI
    mean_proxy_bi = mean(Proxy_BI, na.rm = TRUE),
    median_proxy_bi = median(Proxy_BI, na.rm = TRUE),
    
    # Area
    mean_area = mean(Area, na.rm = TRUE),
    median_area = median(Area, na.rm = TRUE),
    
    # Branches
    mean_branches = mean(`# of branches`, na.rm = TRUE),
    median_branches = median(`# of branches`, na.rm = TRUE),
    
    # End points
    mean_endpoints = mean(`# of end point voxels`, na.rm = TRUE),
    median_endpoints = median(`# of end point voxels`, na.rm = TRUE),
    
    # Circularity
    mean_circularity = mean(Circularity, na.rm = TRUE),
    median_circularity = median(Circularity, na.rm = TRUE),
    
    # Branch length
    mean_branch_length = mean(`Average branch length`, na.rm = TRUE),
    median_branch_length = median(`Average branch length`, na.rm = TRUE),
    
    n_cells = n(),
    .groups = "drop"
  ) %>%
  arrange(Cluster)

print(cluster_comparison)

# Save the comparison
write.csv(cluster_comparison, 
          paste0(output_dir, "cluster_mean_median_comparison.csv"), 
          row.names = FALSE)
# ============================================================





# ============================================================
# AVERAGE CELL COUNTS PER SUBJECT PER BRAIN REGION
# ============================================================
# This section calculates the average number of cells per image
# for each subject in each brain region, accounting for the fact
# that different subjects may have different numbers of images
# per region due to data cleaning/exclusions

cat("\n\n=============================================================\n")
cat("CALCULATING AVERAGE CELL COUNTS PER SUBJECT PER REGION\n")
cat("=============================================================\n\n")

# Step 1: Get the TOTAL number of images per subject per region (regardless of what's in them)
n_images_per_region <- pca_kmeans %>%
  group_by(Mouse_ID, Treatment, Brain_Region) %>%
  summarise(n_images = n_distinct(Image_Number_In_Region), .groups = "drop")

print("=== Number of Images Per Subject Per Region ===")
print(n_images_per_region)

# Step 2: Count total cells per cluster per image
cells_per_image_per_cluster <- pca_kmeans %>%
  group_by(Mouse_ID, Treatment, Brain_Region, Image_Number_In_Region, 
           Cluster) %>%
  summarise(n_cells = n(), .groups = "drop")

# Step 3: Create a complete grid of all possible combinations
# This ensures we have an entry for every cluster in every image, even if count is 0
complete_grid <- pca_kmeans %>%
  distinct(Mouse_ID, Treatment, Brain_Region, Image_Number_In_Region) %>%
  crossing(
    data.frame(
      Cluster = c(1, 2, 3)
    )
  )

# Step 4: Join with actual counts and fill missing with 0
cells_complete <- complete_grid %>%
  left_join(cells_per_image_per_cluster, 
            by = c("Mouse_ID", "Treatment", "Brain_Region", "Image_Number_In_Region",
                   "Cluster")) %>%
  mutate(n_cells = replace_na(n_cells, 0))

# Step 5: Calculate average cells per cluster per subject per region
# Now every cluster is represented in every image (with 0s where appropriate)
avg_cells_by_cluster <- cells_complete %>%
  group_by(Mouse_ID, Treatment, Brain_Region, 
           Cluster) %>%
  summarise(
    total_cells = sum(n_cells),
    avg_cells_per_image = mean(n_cells),
    sd_cells_per_image = sd(n_cells),
    .groups = "drop"
  ) %>%
  arrange(Mouse_ID, Brain_Region, Cluster)

print("=== Average Cells Per Image by Cluster ===")
print(avg_cells_by_cluster)

# Step 6: Calculate total average cells per subject per region (all clusters combined)
avg_total_cells <- cells_complete %>%
  group_by(Mouse_ID, Treatment, Brain_Region, Image_Number_In_Region) %>%
  summarise(total_cells_in_image = sum(n_cells), .groups = "drop") %>%
  group_by(Mouse_ID, Treatment, Brain_Region) %>%
  summarise(
    total_cells_all_images = sum(total_cells_in_image),
    avg_total_cells_per_image = mean(total_cells_in_image),
    sd_total_cells_per_image = sd(total_cells_in_image),
    .groups = "drop"
  ) %>%
  arrange(Mouse_ID, Brain_Region)

print("=== Average Total Cells Per Image (All Clusters) ===")
print(avg_total_cells)

# Step 7: Create a wide-format table for easier viewing
# This should now have EXACTLY one row per subject per region
avg_cells_wide <- avg_cells_by_cluster %>%
  select(Mouse_ID, Treatment, Brain_Region, Cluster, 
         avg_cells_per_image) %>%
  pivot_wider(
    names_from = Cluster,
    values_from = avg_cells_per_image,
    names_prefix = "Avg_Cluster_",
    values_fill = 0
  ) %>%
  # Join in n_images 
  left_join(n_images_per_region, by = c("Mouse_ID", "Treatment", "Brain_Region")) %>%
  # Add total average cells
  left_join(
    avg_total_cells %>% select(Mouse_ID, Brain_Region, avg_total_cells_per_image),
    by = c("Mouse_ID", "Brain_Region")
  ) %>%
  rename(Avg_Total_All_Clusters = avg_total_cells_per_image) %>%
  # Reorder columns for clarity
  select(Mouse_ID, Treatment, Brain_Region, n_images, 
         Avg_Cluster_1, Avg_Cluster_2, Avg_Cluster_3,
         Avg_Total_All_Clusters) %>%
  arrange(Mouse_ID, Brain_Region)

print("=== Wide Format: Average Cells Per Image by Subject and Region ===")
print(avg_cells_wide)

# Verify: Check for any duplicates (should be 0)
duplicates_check <- avg_cells_wide %>%
  group_by(Mouse_ID, Brain_Region) %>%
  filter(n() > 1)

if(nrow(duplicates_check) > 0) {
  cat("\nWARNING: Found duplicate rows:\n")
  print(duplicates_check)
} else {
  cat("\n✓ SUCCESS: No duplicate rows found. Each subject has exactly one row per brain region.\n")
}

# Step 8: Save all results to CSV files
write.csv(avg_cells_by_cluster, 
          paste0(output_dir, "avg_cells_per_image_by_cluster.csv"), 
          row.names = FALSE)
cat("Saved: avg_cells_per_image_by_cluster.csv\n")

write.csv(avg_total_cells, 
          paste0(output_dir, "avg_total_cells_per_image.csv"), 
          row.names = FALSE)
cat("Saved: avg_total_cells_per_image.csv\n")

write.csv(avg_cells_wide, 
          paste0(output_dir, "avg_cells_per_image_wide_format.csv"), 
          row.names = FALSE)
cat("Saved: avg_cells_per_image_wide_format.csv\n")

# Step 9: Create summary statistics across subjects
summary_by_region_treatment <- avg_cells_wide %>%
  group_by(Brain_Region, Treatment) %>%
  summarise(
    n_subjects = n(),
    mean_images_per_subject = mean(n_images),
    
    # Cluster 1
    mean_cluster_1 = mean(Avg_Cluster_1, na.rm = TRUE),
    sd_cluster_1 = sd(Avg_Cluster_1, na.rm = TRUE),
    
    # Cluster 2
    mean_cluster_2 = mean(Avg_Cluster_2, na.rm = TRUE),
    sd_cluster_2 = sd(Avg_Cluster_2, na.rm = TRUE),
    
    # Cluster 3
    mean_cluster_3 = mean(Avg_Cluster_3, na.rm = TRUE),
    sd_cluster_3 = sd(Avg_Cluster_3, na.rm = TRUE),
    
    # Total
    mean_total = mean(Avg_Total_All_Clusters, na.rm = TRUE),
    sd_total = sd(Avg_Total_All_Clusters, na.rm = TRUE),
    
    .groups = "drop"
  )

print("=== Summary: Mean Cells Per Image Across Subjects ===")
print(summary_by_region_treatment)

write.csv(summary_by_region_treatment, 
          paste0(output_dir, "summary_avg_cells_by_region_treatment.csv"), 
          row.names = FALSE)
cat("Saved: summary_avg_cells_by_region_treatment.csv\n")

cat("\n=============================================================\n")
cat("AVERAGE CELL COUNT ANALYSIS COMPLETE!\n")
cat("=============================================================\n")
cat("\nKey outputs:\n")
cat("1. avg_cells_per_image_by_cluster.csv - Detailed breakdown by cluster\n")
cat("2. avg_total_cells_per_image.csv - Total cells per subject per region\n")
cat("3. avg_cells_per_image_wide_format.csv - Easy-to-read wide format (ONE ROW PER SUBJECT PER REGION)\n")
cat("4. summary_avg_cells_by_region_treatment.csv - Group-level summaries\n\n")

cat("\n\n=============================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("=============================================================\n")
cat("\nAll results have been saved to:", output_dir, "\n")



