library(fingertipsR)
library(dplyr)
library(tibble)
library(tidyr)
library(mi)

# Get data
df <- fingertips_data(ProfileID = 19)

# Select as most recent
most_recent_data <- df[grep('2017', df$TimeperiodSortable), ]

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

# Test normality
test_normaility_and_transform <- function(column_data){
  norm_test <- shapiro.test(column_data)
  if (norm_test.p.value < 0.05) {
    norm_test_sr <- shapiro.test(sqrt(column_data))
    if (norm_test_sr.p.value < 0.05) {
      norm_test_lg <- shapiro.test(log(column_data))
      if (norm_test_lg.p.value < 0.05) {
        norm_test_bc <- shapiro.test(boxcox(column_data))
        if (norm_test_bc.p.value < 0.05) {
          return(NA)
        }
        else {
          return(norm_test_bc)
        }
      else {
        return(norm_test_lg)
      }
      }
    }
    else {
      return(norm_test_sr)
    }
  }
  else {
    return(column_data)
  }
}

# Test colinearity