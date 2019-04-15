library(fingertipsR)
library(dplyr)
library(tibble)
library(tidyr)
library(mi)
library(car)
library(ggplot2)
library(ggcorrplot)
library(caret)
library(psycho)

# Get data
df <- fingertips_data(ProfileID = 19, rank = TRUE)
df <- social_determinants_df

# Create a data frame of indicator polarities
polarities <- df %>%
  select(IndicatorID, Polarity) %>%
  distinct() %>%
  spread(key = IndicatorID, value = Polarity)
  

# Select as most recent
most_recent_data <- df %>%
  group_by(IndicatorID) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable)) %>%
  ungroup()

# Select County & UA
most_recent_data <- most_recent_data[grep('County', most_recent_data$AreaType), ]


# Transpose to wide data
wide_df <- most_recent_data %>%
  select(AreaCode, IndicatorID, Value) %>%
  tibble::rowid_to_column() %>%
  group_by(IndicatorID, AreaCode) %>%
  spread(key = IndicatorID, value = Value) %>%
  summarise_all(funs(na.omit(.)[1]))
rownames(wide_df) <- wide_df$AreaCode
wide_df <- wide_df %>%
  select(-rowid) # Remove row id column

# Remove areas with over 50% missing data - areas with this are unlikely to add any information as their values are more imputed than not
wide_df$proportna <- apply(wide_df, 1, function(x) sum(is.na(x)) / dim(wide_df)[2])
wide_df <- wide_df[wide_df$proportna < 0.5,] 
wide_df <- wide_df %>% select(-proportna)


# Remove indicators with over 80% missing data - rather arbatrary number at the moment
wide_df <- wide_df[colSums(is.na(wide_df)) / dim(wide_df)[1] < 0.2]


# impute missing data - using a Bayesian framework, with 30 inerations and 4 chains. A random seed was chosen to ensure reproducability. The bootstrap was envoked as a method of selecting random imputations. 
# http://www.stat.columbia.edu/~gelman/research/published/mipaper.pdf
to_impute <- wide_df
rownames(to_impute) <- to_impute$AreaCode
to_impute <- to_impute %>% select(-AreaCode) 
imputed <- mi(as.data.frame(to_impute), seed = 225)
summary(imputed)
image(imputed) # Show heatmap of imputed values in heatmap
imputed_df <- mi::complete(imputed, m = 1) # Retrieve the dataframe
imputed_df <- select(imputed_df, -contains("missing")) # Remove the boolean 'missing' columns. 


# Test normality - using shapiro-wilks with an alpha of 0.001. Alpha chosen as Central Limit Theorum would suggest that with n=150, data should tend towards being normally distributed. 
# These tests are to catch extremes where normality is violated and to transform them to normalise using square root, log, and box-cox transformations in turn. The box-cox transformation
# used was the Yeo-Johnson variation (yjPower()) as some of the data contains 0 values.
# Yeo, In-Kwon and Johnson, Richard (2000) A new family of power transformations to improve normality or symmetry. Biometrika, 87, 954-959.
test_normaility_and_transform <- function(column_data){

  norm_test <- shapiro.test(column_data)
  print(norm_test)
  if (norm_test$p.value < 0.001) {
    norm_test_sr <- shapiro.test(sqrt(column_data))
    if (norm_test_sr$p.value < 0.001) {
      column_data[column_data == 0] <- 0.00001
      if (is.numeric(column_data == FALSE)){
        print(column_data)
      }
      norm_test_lg <- shapiro.test(log(column_data))
      if (norm_test_lg$p.value < 0.001) {
        
        norm_test_bc <- shapiro.test(yjPower(column_data, lambda = 1))
        
        print(norm_test_bc)
        if (norm_test_bc$p.value < 0.001) {
          return(NA)
        } 
        else { return(yjPower(column_data, lambda = 1))} }
      else {
        return(log(column_data))
      }
    } 
    else {
      return(sqrt(column_data))
    }
  }
  else {
    return(column_data)
  }
}


normality <- imputed_df %>% summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, p.value = shapiro.test(.)$p.value))
# Normalise data.
# normalised <- data.frame(imputed_df, lapply(imputed_df, test_normaility_and_transform))
# 
# #Some transformed data contains NA values, and therefore these values were imputed using the same method as above. 
# norm_reduced <- select(normalised, contains(".1"))
# norm_reduced <- norm_reduced[!sapply(norm_reduced, function(x) all(is.na(x)))]
# norm_reduced <- mi(norm_reduced, seed = 225)
# summary(norm_reduced)
# image(norm_reduced)
# norm_reduced <- mi::complete(norm_reduced, m = 1)
# norm_reduced <- select(norm_reduced, -contains("missing"))
norm_reduced <- imputed_df

# Test colinearity - needs a lot of work!
corr <- cor(norm_reduced)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           insig = "blank")
hc <- findCorrelation(corr)
hc <- sort(hc)
reduced_data <- norm_reduced[, -c(hc)]
corr_reduced <- cor(reduced_data)
ggcorrplot(corr_reduced, hc.order = TRUE, type = "lower",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           insig = "blank")


# Polarity check and inversion. If 'Low is good', then the scores are inverted to ensure the same direction of performance in the data. 
polarity_check <- function(col_name){
  if (grepl(polarities[[col_name]], "RAG - Low is good")) {
    z_data[[col_name]] <- -z_data[[col_name]]
  } else {
    z_data[[col_name]] <- z_data[[col_name]]
  }
}


# Create Z scores using psycho package's standardize() function. 
# Makowski, (2018). The psycho Package: an Efficient and Publishing-Oriented Workflow for Psychological Science. Journal of Open Source Software, 3(22), 470. https://doi.org/10.21105/joss.00470
z_data <- reduced_data %>% standardize()
names(z_data) <- substring(names(z_data), 2)
z_data <- z_data %>% 
              rename_at(.vars = vars(ends_with(".1")),
              .funs = funs(sub("[.]1$", "", .)))
polarities <- polarities[, colnames(z_data)]


# Create a dataframe of z-scores, create mean score & add area codes on. 
z_data <- data.frame(z_data, lapply(colnames(z_data), polarity_check))
z_data <- z_data[, -c(1:ncol(polarities))]
colnames(z_data) <- colnames(polarities)
z_data$mean <- apply(z_data, 1, mean)
z_data$AreaCode <- wide_df$AreaCode


z_mean <- z_data %>% select(mean, AreaCode)
