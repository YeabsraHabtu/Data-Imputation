# Load Required Libraries
library(tidyverse)
library(mice)
library(VIM)
library(naniar)
library(visdat)
library(summarytools)
library(data.table)
library(magrittr)
library(readr)
library(purrr)
library(dplyr)
library(tools)
library(MissMech)
library(readxl) 
library(corrplot)
library(ggplot2)

print("All packages loaded successfully!")

# Set folder paths (for climate and powerplant only)
folder_paths <- c(
  "C:/Users/Aimable/Documents/RSTUDIO/Data/Climate",
  "C:/Users/Aimable/Documents/RSTUDIO/Data/Global"
)

# Define import function for CSVs
import_folder <- function(folder) {
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  names(csv_files) <- tools::file_path_sans_ext(basename(csv_files))
  map(csv_files, read_csv)
}

# Import climate and powerplant data
all_data <- map(folder_paths, import_folder) %>%
  set_names(c("climate", "powerplant"))

# Load the cleaned renewable Excel sheet separately
renewable_data <- list(read_excel("C:/Users/Aimable/Documents/RSTUDIO/Data/Renewable/Renewable_Cleaned.xlsx"))

# Extract datasets
climate_data <- all_data$climate
powerplant_data <- all_data$powerplant

# Save the datasets
save(climate_data, file = "climate_data.RData")
save(powerplant_data, file = "powerplant_data.RData")
save(renewable_data, file = "renewable_data.RData")

print("Successfully imported and saved the datasets!")



# How many rows to sample (random)
max_rows <- 10000

# Helper: given a data.frame, sample it and return a ggplot of missing counts
plot_missing_bar <- function(df, title, file) {
  samp <- df %>% slice_sample(n = min(nrow(df), max_rows))
  miss <- sapply(samp, function(x) sum(is.na(x)))
  miss_df <- tibble(
    variable = names(miss),
    missing  = as.integer(miss)
  )
  
  p <- ggplot(miss_df, aes(x = reorder(variable, -missing), y = missing)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = title, x = NULL, y = "Missing Count") +
    theme_minimal(base_size = 12)
  
  ggsave(file, plot = p, width = 8, height = 5)
}

# 1. Climate (five CSVs → five sample plots, but let’s combine into one if you prefer)
for (i in seq_along(climate_data)) {
  plot_missing_bar(
    climate_data[[i]],
    title = paste0("Climate Dataset ", i, " Missingness"),
    file  = paste0("climate_missing_", i, ".png")
  )
}

# 2. Powerplant (single CSV)
plot_missing_bar(
  powerplant_data[[1]],
  title = "Powerplant Dataset Missingness",
  file  = "powerplant_missingness.png"
)

# 3. Renewable (single CSV)
plot_missing_bar(
  renewable_data[[1]],
  title = "Renewable Dataset Missingness",
  file  = "renewable_missingness.png"
)


# Imputation Function
imputed_with_mice <- function(df, m = 2) {
  numeric_df <- select(df, where(is.numeric))  # keep only numeric columns
  
  # Remove columns with no variance
  numeric_df <- numeric_df[, sapply(numeric_df, function(x) length(unique(x)) > 1)]
  
  # Apply MICE imputation
  imputed <- mice(numeric_df, m = m, maxit = 2, method = "pmm", seed = 123)
  
  # Return completed data
  complete(imputed, 1)
}

# Impute Missing Values
climate_imputed <- map(climate_data, imputed_with_mice)
powerplant_imputed <- map(powerplant_data, imputed_with_mice)
renewable_imputed <- map(renewable_data, imputed_with_mice)

# Save imputed data
save(climate_imputed, file = "climate_imputed.RData")
save(powerplant_imputed, file = "powerplant_imputed.RData")
save(renewable_imputed, file = "renewable_imputed.RData")

print("All datasets imputed and saved successfully!")

####Save comparision image for original climate data and the imputed one###
numeric_original<-dplyr::select(climate_data[[1]], where(is.numeric))
numeric_imputed<-dplyr::select(climate_imputed[[1]], where(is.numeric))
cor_original<-cor(numeric_original, use='complete.obs') 
cor_imputed<-cor(numeric_imputed, use='complete.obs')

png('correlation-plot1.png', width=1000, height=600)
par(mfrow=c(1,2))
corrplot(cor_original, method='color', type = 'full', title='Original Data',mar=c(0,0,2,0),tl.cex=0.8, addCoef.col='black')
corrplot(cor_imputed, method='color', type = 'full', title='Imputed Data',mar=c(0,0,2,0),tl.cex=0.8, addCoef.col='black')
dev.off()

numeric_original<-dplyr::select(climate_data[[2]], where(is.numeric))
numeric_imputed<-dplyr::select(climate_imputed[[2]], where(is.numeric))
cor_original<-cor(numeric_original, use='complete.obs') 
cor_imputed<-cor(numeric_imputed, use='complete.obs')

png('correlation-plot2.png', width=1000, height=600)
par(mfrow=c(1,2))
corrplot(cor_original, method='color', type = 'full', title='Original Data',mar=c(0,0,2,0),tl.cex=0.8, addCoef.col='black')
corrplot(cor_imputed, method='color', type = 'full', title='Imputed Data',mar=c(0,0,2,0),tl.cex=0.8, addCoef.col='black')
dev.off()

numeric_original<-dplyr::select(climate_data[[3]], where(is.numeric))
numeric_imputed<-dplyr::select(climate_imputed[[3]], where(is.numeric))
cor_original<-cor(numeric_original, use='complete.obs') 
cor_imputed<-cor(numeric_imputed, use='complete.obs')

png('correlation-plot3.png', width=1000, height=600)
par(mfrow=c(1,2))
corrplot(cor_original, method='color', type = 'full', title='Original Data',mar=c(0,0,2,0),tl.cex=0.8, addCoef.col='black')
corrplot(cor_imputed, method='color', type = 'full', title='Imputed Data',mar=c(0,0,2,0),tl.cex=0.8, addCoef.col='black')
dev.off()

numeric_original<-dplyr::select(climate_data[[4]], where(is.numeric))
numeric_imputed<-dplyr::select(climate_imputed[[4]], where(is.numeric))
cor_original<-cor(numeric_original, use='complete.obs') 
cor_imputed<-cor(numeric_imputed, use='complete.obs')

png('correlation-plot4.png', width=1000, height=600)
par(mfrow=c(1,2))
corrplot(cor_original, method='color', type = 'full', title='Original Data',mar=c(0,0,2,0),tl.cex=0.8, addCoef.col='black')
corrplot(cor_imputed, method='color', type = 'full', title='Imputed Data',mar=c(0,0,2,0),tl.cex=0.8, addCoef.col='black')
dev.off()

numeric_original<-dplyr::select(climate_data[[5]], where(is.numeric))
numeric_imputed<-dplyr::select(climate_imputed[[5]], where(is.numeric))
cor_original<-cor(numeric_original, use='complete.obs') 
cor_imputed<-cor(numeric_imputed, use='complete.obs')

png('correlation-plot5.png', width=1000, height=600)
par(mfrow=c(1,2))
corrplot(cor_original, method='color', type = 'full', title='Original Data',mar=c(0,0,2,0),tl.cex=0.8, addCoef.col='black')
corrplot(cor_imputed, method='color', type = 'full', title='Imputed Data',mar=c(0,0,2,0),tl.cex=0.8, addCoef.col='black')
dev.off()



### save correlation result of the original and imputed powerplant dataset##
numeric_original<-dplyr::select(powerplant_data[[1]], where(is.numeric))
numeric_imputed<-dplyr::select(powerplant_imputed[[1]], where(is.numeric))
cor_original<-cor(numeric_original, use='pairwise.complete.obs') 
cor_imputed<-cor(numeric_imputed, use='pairwise.complete.obs')

png('correlation-plot6.png', width=2000, height=800)
par(mfrow=c(1,2))
corrplot(cor_original, method='color', type = 'full', title='Original Data',mar=c(0,0,3,0),tl.cex=0.9, addCoef.col='black')
corrplot(cor_imputed, method='color', type = 'full', title='Imputed Data',mar=c(0,0,3,0),tl.cex=0.9, addCoef.col='black')
dev.off()


### save correlation result of the original and imputed renewable dataset##
numeric_original<-dplyr::select(renewable_data[[1]], where(is.numeric))
numeric_imputed<-dplyr::select(renewable_imputed[[1]], where(is.numeric))
cor_original<-cor(numeric_original, use='pairwise.complete.obs') 
cor_imputed<-cor(numeric_imputed, use='pairwise.complete.obs')

png('correlation-plot7.png', width=1200, height=600)
par(mfrow=c(1,2))
corrplot(cor_original, method='color', type = 'full', title='Original Data',mar=c(0,0,2,0),tl.cex=0.9, addCoef.col='black')
corrplot(cor_imputed, method='color', type = 'full', title='Imputed Data',mar=c(0,0,2,0),tl.cex=0.9, addCoef.col='black')
dev.off()