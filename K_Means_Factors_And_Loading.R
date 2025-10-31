##THIS SCRIPT IS MEANT TO BE RUN AFTER K_MEANS_PIPELINE to see what features make up
##the majority of variance and in the PC analysis (loading, etc). I've ran this
##and determined that proxy_bi is a good measure for univariate feature clustering.
## This script uses the same output_dir as defined in your K_Means_Pipeline


# ============================================================
# PC-FEATURE CORRELATION AND FACTOR ANALYSIS
# ============================================================

library(corrplot)
library(psych)  # For factor analysis and rotation
library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# 1. EXTRACT PC LOADINGS (CORRELATIONS)
# ============================================================

# Get the PCA object from your transformed data
pca_object <- prcomp(finaldata_with_indices_logtransformed[, 8:36], 
                     center = TRUE, 
                     scale. = TRUE)

# Extract loadings (rotation matrix)
loadings <- pca_object$rotation

# Calculate correlations between original features and PCs
# This is what you see in the biplot
feature_pc_correlations <- cor(finaldata_with_indices_logtransformed[, 8:36], 
                               pca_data[, 1:5])

# Create a detailed loadings table
loadings_df <- as.data.frame(loadings[, 1:5])
loadings_df$Feature <- rownames(loadings_df)
loadings_df <- loadings_df %>%
  select(Feature, everything()) %>%
  arrange(desc(abs(PC1)))  # Sort by absolute PC1 contribution

print("=== PC LOADINGS (Top Contributors to Each PC) ===")
print(loadings_df)

# Save to CSV
write.csv(loadings_df, 
          paste0(output_dir, "pc_loadings.csv"), 
          row.names = FALSE)

# ============================================================
# 2. FEATURE-PC CORRELATIONS WITH SIGNIFICANCE
# ============================================================

# Function to calculate correlation with p-value
cor_with_pvalue <- function(x, y) {
  test <- cor.test(x, y)
  return(c(r = test$estimate, p = test$p.value))
}

# Calculate correlations for each feature-PC pair
correlation_results <- data.frame()

for (pc_num in 1:5) {
  pc_name <- paste0("PC", pc_num)
  
  for (feat_idx in 8:36) {
    feat_name <- names(finaldata_with_indices_logtransformed)[feat_idx]
    
    cor_result <- cor_with_pvalue(
      finaldata_with_indices_logtransformed[[feat_idx]], 
      pca_data[[pc_num]]
    )
    
    correlation_results <- rbind(correlation_results, 
                                 data.frame(
                                   PC = pc_name,
                                   Feature = feat_name,
                                   Correlation = cor_result["r.cor"],
                                   P_value = cor_result["p.p-value"],
                                   Abs_Correlation = abs(cor_result["r.cor"])
                                 ))
  }
}

# Add significance flags
correlation_results <- correlation_results %>%
  mutate(
    Significant = ifelse(P_value < 0.05, "***", 
                         ifelse(P_value < 0.1, "*", "")),
    Bonferroni_Sig = ifelse(P_value < 0.05/nrow(correlation_results), "YES", "NO")
  ) %>%
  arrange(PC, desc(Abs_Correlation))

print("=== FEATURE-PC CORRELATIONS (with significance) ===")
print(head(correlation_results, 20))

write.csv(correlation_results, 
          paste0(output_dir, "feature_pc_correlations.csv"), 
          row.names = FALSE)

# Top features for each PC
top_features_per_pc <- correlation_results %>%
  filter(Significant != "") %>%
  group_by(PC) %>%
  slice_max(order_by = Abs_Correlation, n = 5) %>%
  arrange(PC, desc(Abs_Correlation))

print("=== TOP 5 FEATURES PER PC (significant only) ===")
print(top_features_per_pc)

write.csv(top_features_per_pc, 
          paste0(output_dir, "top_features_per_pc.csv"), 
          row.names = FALSE)

# ============================================================
# 3. VARIANCE EXPLAINED BY EACH PC
# ============================================================

variance_explained <- data.frame(
  PC = paste0("PC", 1:5),
  Standard_Deviation = pca_object$sdev[1:5],
  Variance = pca_object$sdev[1:5]^2,
  Proportion_of_Variance = (pca_object$sdev[1:5]^2) / sum(pca_object$sdev^2),
  Cumulative_Proportion = cumsum((pca_object$sdev[1:5]^2) / sum(pca_object$sdev^2))
)

print("=== VARIANCE EXPLAINED BY PCs ===")
print(variance_explained)

write.csv(variance_explained, 
          paste0(output_dir, "variance_explained.csv"), 
          row.names = FALSE)

# ============================================================
# 4. CLUSTER-FEATURE RELATIONSHIPS
# ============================================================

# Calculate mean values for each feature by cluster
cluster_feature_means <- pca_kmeans %>%
  group_by(Cluster) %>%
  summarise(across(10:38, mean, na.rm = TRUE))

print("=== MEAN FEATURE VALUES BY CLUSTER ===")
print(cluster_feature_means)

write.csv(cluster_feature_means, 
          paste0(output_dir, "cluster_feature_means.csv"), 
          row.names = FALSE)

# Statistical tests: ANOVA for each feature across clusters
anova_results <- data.frame()

for (feat_idx in 10:38) {
  feat_name <- names(pca_kmeans)[feat_idx]
  
  # Perform ANOVA
  formula_str <- paste("`", feat_name, "` ~ Cluster", sep = "")
  anova_test <- aov(as.formula(formula_str), data = pca_kmeans)
  anova_summary <- summary(anova_test)
  
  # Extract F-statistic and p-value
  f_stat <- anova_summary[[1]]$`F value`[1]
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  
  anova_results <- rbind(anova_results, 
                         data.frame(
                           Feature = feat_name,
                           F_statistic = f_stat,
                           P_value = p_value,
                           Significant = ifelse(p_value < 0.05, "***", 
                                                ifelse(p_value < 0.1, "*", ""))
                         ))
}

anova_results <- anova_results %>%
  arrange(P_value)

print("=== ANOVA: FEATURES MOST DIFFERENT ACROSS CLUSTERS ===")
print(head(anova_results, 15))

write.csv(anova_results, 
          paste0(output_dir, "cluster_anova_results.csv"), 
          row.names = FALSE)

# ============================================================
# 5. FACTOR ANALYSIS WITH ROTATION
# ============================================================

# Perform factor analysis with varimax rotation (orthogonal)
# This maximizes interpretability by making loadings more extreme
fa_varimax <- fa(finaldata_with_indices_logtransformed[, 8:36], 
                 nfactors = 5, 
                 rotate = "varimax", 
                 fm = "ml")  # Maximum likelihood

print("=== FACTOR ANALYSIS (VARIMAX ROTATION) ===")
print(fa_varimax$loadings, cutoff = 0.3)  # Only show loadings > 0.3

# Extract rotated loadings
fa_loadings_varimax <- as.data.frame(fa_varimax$loadings[])
fa_loadings_varimax$Feature <- rownames(fa_loadings_varimax)
fa_loadings_varimax <- fa_loadings_varimax %>%
  select(Feature, everything())

write.csv(fa_loadings_varimax, 
          paste0(output_dir, "factor_loadings_varimax.csv"), 
          row.names = FALSE)

# Oblique rotation (allows factors to correlate)
fa_oblimin <- fa(finaldata_with_indices_logtransformed[, 8:36], 
                 nfactors = 5, 
                 rotate = "oblimin", 
                 fm = "ml")

print("=== FACTOR ANALYSIS (OBLIMIN ROTATION - allows correlation) ===")
print(fa_oblimin$loadings, cutoff = 0.3)

fa_loadings_oblimin <- as.data.frame(fa_oblimin$loadings[])
fa_loadings_oblimin$Feature <- rownames(fa_loadings_oblimin)
fa_loadings_oblimin <- fa_loadings_oblimin %>%
  select(Feature, everything())

write.csv(fa_loadings_oblimin, 
          paste0(output_dir, "factor_loadings_oblimin.csv"), 
          row.names = FALSE)

# Factor correlations (for oblimin)
if (!is.null(fa_oblimin$Phi)) {
  print("=== FACTOR CORRELATIONS (Oblimin) ===")
  print(fa_oblimin$Phi)
  
  write.csv(fa_oblimin$Phi, 
            paste0(output_dir, "factor_correlations.csv"))
}

# ============================================================
# 6. COMMUNALITIES (HOW WELL EACH FEATURE IS EXPLAINED)
# ============================================================

communalities <- data.frame(
  Feature = names(finaldata_with_indices_logtransformed[, 8:36]),
  Communality_Varimax = fa_varimax$communality,
  Uniqueness_Varimax = fa_varimax$uniquenesses,
  Communality_Oblimin = fa_oblimin$communality
) %>%
  arrange(desc(Communality_Varimax))

print("=== COMMUNALITIES (How well features are explained by factors) ===")
print(communalities)

write.csv(communalities, 
          paste0(output_dir, "communalities.csv"))

# ============================================================
# 7. SCREE PLOT FOR FACTOR RETENTION
# ============================================================

png(paste0(output_dir, "scree_plot.png"), width = 800, height = 600)
scree(finaldata_with_indices_logtransformed[, 8:36], factors = FALSE)
dev.off()

# Parallel analysis to determine optimal number of factors
parallel_result <- fa.parallel(finaldata_with_indices_logtransformed[, 8:36], 
                               fm = "ml", 
                               fa = "fa")

print("=== PARALLEL ANALYSIS SUGGESTS ===")
print(paste("Number of factors:", parallel_result$nfact))
print(paste("Number of components:", parallel_result$ncomp))

# ============================================================
# 8. IDENTIFY TOP DISCRIMINATING FEATURES FOR FUTURE TRAINING
# ============================================================

# Combine multiple criteria
top_features <- correlation_results %>%
  filter(Significant != "") %>%
  group_by(Feature) %>%
  summarise(
    Max_PC_Correlation = max(Abs_Correlation),
    Avg_PC_Correlation = mean(Abs_Correlation),
    .groups = "drop"
  ) %>%
  left_join(
    anova_results %>% select(Feature, F_statistic, ANOVA_Pvalue = P_value),
    by = "Feature"
  ) %>%
  left_join(
    communalities %>% select(Feature, Communality_Varimax),
    by = "Feature"
  ) %>%
  mutate(
    # Composite score: high PC correlation + high cluster discrimination
    Composite_Score = scale(Max_PC_Correlation) + 
      scale(-log10(ANOVA_Pvalue + 1e-10)) +
      scale(Communality_Varimax)
  ) %>%
  arrange(desc(Composite_Score))

print("=== TOP FEATURES FOR FUTURE TRAINING (composite ranking) ===")
print(head(top_features, 15))

write.csv(top_features, 
          paste0(output_dir, "top_features_for_training.csv"), 
          row.names = FALSE)

# ============================================================
# 9. VISUALIZATION: HEATMAP OF LOADINGS
# ============================================================

# Varimax loadings heatmap
png(paste0(output_dir, "loadings_heatmap_varimax.png"), 
    width = 1000, height = 1200)
corrplot(as.matrix(fa_loadings_varimax[, 2:6]), 
         is.corr = FALSE,
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Factor Loadings (Varimax Rotation)",
         mar = c(0,0,2,0))
dev.off()

print("=== ANALYSIS COMPLETE ===")
print(paste("All results saved to:", output_dir))