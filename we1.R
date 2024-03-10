dib <- data
# Function to remove outliers using IQR
remove_outliers_iqr <- function(df) {
  q <- apply(df, 2, quantile, c(0.25, 0.75))
  iqr <- q[2, ] - q[1, ]
  lower_bound <- q[1, ] - 1.5 * iqr
  upper_bound <- q[2, ] + 1.5 * iqr
  outliers <- apply(df, 2, function(col) col < lower_bound | col > upper_bound)
  df[!apply(outliers, 1, any), ]
}

# Remove outliers using IQR
df_no_outliers_iqr <- remove_outliers_iqr(dib)
# Set a frequency threshold for each category
threshold <- 2

# Function to filter the dataframe based on frequency thresholds
filter_by_frequency <- function(df, threshold) {
  categories_to_keep <- sapply(df, function(col) {
    category_counts <- table(col)
    names(category_counts)[category_counts >= threshold]
  })
  
  df_filtered <- df
  for (col in names(df)) {
    df_filtered <- df_filtered[df_filtered[[col]] %in% categories_to_keep[[col]], , drop = FALSE]
  }
  
  return(df_filtered)
}


# Remove rows with rare combinations of categories
df_filtered <- filter_by_frequency(dib, threshold)
df_filtered


#remove the outliers
outliers_remover <- function(a){
  df <- a
  aa <- c()
  count <- 1
  for(i in 1:ncol(df)){
    if(is.integer(df[,i])){
      Q3 <- quantile(df[,i], 0.75, na.rm = TRUE)
      Q1 <- quantile(df[,i], 0.25, na.rm = TRUE) 
      IQR <- Q3 - Q1  #IQR(df[,i])
      upper <- Q3 + 1.5 * IQR
      lower <- Q1 - 1.5 * IQR
      for(j in 1:nrow(df)){
        if(is.na(df[j,i]) == TRUE){
          next
        }
        else if(df[j,i] > upper | df[j,i] < lower){
          aa[count] <- j
          count <- count+1                  
        }
      }
    }
  }
  df <- df[-aa,]
}
st <-outliers_remover(dib)
boxplot(st$num_lab_procedures)
######################
library(caret)
preprocess <- preProcess(dib[1:46], method = c("center","scale"))
print(preprocess)
# transform the dataset using the parameters
transformed <- predict(preprocess, dib[,1:46])
transformed
dim(transformed)
################################
library(vtreat)

# Create a treatment plan for the data
library(mltools)

library(data.table)
data$gender <- 
dibd <- one_hot((dib))

dummy <- dummyVars(" ~ .", data=dib,)
newdata <- data.frame(predict(dummy, newdata = dib))
newdata





# Sample dataset
data <- data.frame(
  age_group = c("<30", ">30", "NO", ">30", "<30", "NO", ">30", "<30", "NO"),
  diagnosis = c("A", "B", "A", "C", "B", "A", "C", "A", "B"),
  other_column = c(1, 2, 3, 1, 2, 3, 1, 2, 3)
)

# Apply label encoding to all categorical columns
categorical_columns <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]
for (col in categorical_columns) {
  data <- label_encode_column(data, col)
}

# Display the result
print(data)
## new ways 
plot(data$age, main = "age distribution") # age: mode 70-80yrs normal distribution, right skewed
plot(data$gender, main = "gender distribution") # gender: female 53% male 47%
plot(data$A1Cresult, main = "A1C") # A1Cresult: 84% no A1c results, 8% >8
plot(data$readmitted, main = "readmissions") # readmission: >50% no readmission
plot(data$admission_source, main = "admission source") # emergency 60%

s <- data

# Perform one-hot encoding for categorical columns
categorical_columns <- names(s)[sapply(s, is.factor) | sapply(s, is.character)]
encoded_data <- s

for (col in categorical_columns) {
  if (is.factor(s[[col]]) | is.character(s[[col]])) {
    encoded_cols <- model.matrix(~ s[[col]] - 1, data = s)
    colnames(encoded_cols) <- paste0(col, "_", colnames(encoded_cols))
    encoded_data <- cbind(encoded_data, encoded_cols)
  }
}

# Remove the original categorical columns
encoded_data <- encoded_data[, !sapply(encoded_data, is.factor) & !sapply(encoded_data, is.character), drop = FALSE]

# Display the result
print(encoded_data)

