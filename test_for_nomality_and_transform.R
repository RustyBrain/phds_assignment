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
polarities <- df %>%
  select(IndicatorID, Polarity) %>%
  distinct() %>%
  spread(key = IndicatorID, value = Polarity)
  

# Select as most recent
most_recent_data <- df[grep('2017', df$TimeperiodSortable), ]

# Select County & UA
most_recent_data <- most_recent_data[grep('County', most_recent_data$AreaType), ]

# check for polarity of indicators


# Transpose to wide data
wide_df <- most_recent_data %>%
  select(AreaCode, IndicatorID, Value) %>%
  tibble::rowid_to_column() %>%
  group_by(IndicatorID, AreaCode) %>%
  spread(key = IndicatorID, value = Value) %>%
  summarise_all(funs(na.omit(.)[1]))
rownames(wide_df) <- wide_df$AreaCode
wide_df <- wide_df %>%
  select(-rowid)

# Remove areas with over 50% missing data
wide_df$proportna <- apply(wide_df, 1, function(x) sum(is.na(x)) / dim(wide_df)[2])
wide_df <- wide_df[wide_df$proportna < 0.5,] 
wide_df <- wide_df %>% select(-proportna)


# Remove indicators with over 80% missing data
wide_df <- wide_df[colSums(is.na(wide_df)) / dim(wide_df)[1] < 0.2]

# impute missing data

to_impute <- wide_df
rownames(to_impute) <- to_impute$AreaCode
to_impute <- to_impute %>% select(-AreaCode) 

imputed <- mi(as.data.frame(to_impute), seed = 225)
summary(imputed)
image(imputed)
imputed_df <- complete(imputed, m = 1)
imputed_df <- select(imputed_df, -contains("missing"))

# Test normality

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

normalised <- data.frame(imputed_df, lapply(imputed_df, test_normaility_and_transform))

norm_reduced <- select(normalised, contains(".1"))
norm_reduced <- norm_reduced[!sapply(norm_reduced, function(x) all(is.na(x)))]
norm_reduced <- mi(norm_reduced, seed = 225)
norm_reduced <- complete(norm_reduced, m = 1)
norm_reduced <- select(norm_reduced, -contains("missing"))

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

# polarity check and inversion

polarity_check <- function(col_name){
  if (grepl(polarities[[col_name]], "RAG - Low is good")) {
    z_data[[col_name]] <- -z_data[[col_name]]
  } else {
    z_data[[col_name]] <- z_data[[col_name]]
  }
}

# create Z scores
z_data <- reduced_data %>% standardize()
names(z_data) <- substring(names(z_data), 2)
z_data <- z_data %>% 
              rename_at(.vars = vars(ends_with(".1")),
              .funs = funs(sub("[.]1$", "", .)))
polarities <- polarities[, colnames(z_data)]
z_data <- data.frame(z_data, lapply(colnames(z_data), polarity_check))
z_data <- z_data[, -c(1:ncol(polarities))]
colnames(z_data) <- colnames(polarities)

z_data$mean <- apply(z_data, 1, mean)
z_data$AreaCode <- wide_df$AreaCode

z_mean <- z_data %>% select(mean, AreaCode)
