#library needed
library(dplyr)
library()

#load data 
train = read.csv("train.csv",stringsAsFactors = F)
test = read.csv("test.csv",stringsAsFactors = F)

#summary 
summary(train)

sum(is.na(train))
#checking value with na
colSums(is.na(train))
# checking value with na for test 
colSums(is.na(test))
## fill na with zero 
train[is.na(train)] <- 0
test[is.na(test)] <- 0


dim(train)
dim(test)



# selecxt x and y \
X <- train$GrLivArea
Y <-  train$SalePrice

# Load the required library
library(ggplot2)

# Create a histogram for X (independent variable)
ggplot(data = NULL, aes(x = X)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "black") +
  labs(title = "Histogram of GrLivArea (Independent Variable)",
       x = "GrLivArea",
       y = "Frequency") +
  theme_minimal()

# Create a density plot for Y (dependent variable)
ggplot(data = NULL, aes(x = Y)) +
  geom_density(fill = "lightgreen", color = "black") +
  labs(title = "Density Plot of SalePrice (Dependent Variable)",
       x = "SalePrice",
       y = "Density") +
  theme_minimal()

########## probability question 
# Calculate the 3rd quartile of X and the 2nd quartile of Y
x <- quantile(X, probs = 0.75, na.rm = TRUE)
y <- quantile(Y, probs = 0.50, na.rm = TRUE)

# Count observations satisfying each condition
# a. P(X > x | Y > y)
count_a <- sum(X > x & Y > y)

# b. P(X > x, Y > y)
count_b <- sum(X > x, Y > y)

# c. P(X < x | Y > y)
count_c <- sum(X < x & Y > y)

# Total count of observations
total_count <- length(X)

# Calculate probabilities
prob_a <- count_a / total_count
prob_b <- count_b / total_count
prob_c <- count_c / total_count

# Interpretation of probabilities
# a. P(X > x | Y > y): Probability that X is greater than its 3rd quartile given Y is greater than its 2nd quartile.
# b. P(X > x, Y > y): Probability that both X and Y are greater than their respective quartiles.
# c. P(X < x | Y > y): Probability that X is less than its 3rd quartile given Y is greater than its 2nd quartile.

# Print probabilities
cat("a. P(X > x | Y > y) =", prob_a, "\n")
cat("b. P(X > x, Y > y) =", prob_b, "\n")
cat("c. P(X < x | Y > y) =", prob_c, "\n")

# Create a table of counts
table_counts <- matrix(c(count_a, count_b, count_c), nrow = 1, byrow = TRUE)
colnames(table_counts) <- c("Count(X > x | Y > y)", "Count(X > x, Y > y)", "Count(X < x | Y > y)")
rownames(table_counts) <- "Counts"
print(table_counts)



# Calculate the 2nd quartile of X and Y
x_quartile2 <- quantile(X, probs = 0.50, na.rm = TRUE)
y_quartile2 <- quantile(Y, probs = 0.50, na.rm = TRUE)

# Calculate the 3rd quartile of X
x_quartile3 <- quantile(X, probs = 0.75, na.rm = TRUE)

# Categorize observations based on quartiles
x_leq_2d_quartile <- sum(X <= x_quartile2)
x_gt_2d_quartile <- sum(X > x_quartile2)
x_leq_3d_quartile <- sum(X <= x_quartile3)
x_gt_3d_quartile <- sum(X > x_quartile3)

# Calculate counts for each category
leq_2d_quartile_leq_3d_quartile <- sum(X <= x_quartile2 & Y <= y_quartile2)
leq_2d_quartile_gt_3d_quartile <- sum(X <= x_quartile2 & Y > y_quartile2)
gt_2d_quartile_leq_3d_quartile <- sum(X > x_quartile2 & Y <= y_quartile2)
gt_2d_quartile_gt_3d_quartile <- sum(X > x_quartile2 & Y > y_quartile2)

# Calculate totals
total_leq_3d_quartile <- x_leq_3d_quartile
total_gt_3d_quartile <- x_gt_3d_quartile
total_leq_2d_quartile <- sum(leq_2d_quartile_leq_3d_quartile, leq_2d_quartile_gt_3d_quartile)
total_gt_2d_quartile <- sum(gt_2d_quartile_leq_3d_quartile, gt_2d_quartile_gt_3d_quartile)
total <- total_leq_3d_quartile + total_gt_3d_quartile

# Fill out the table
table_counts <- matrix(c(
  leq_2d_quartile_leq_3d_quartile, leq_2d_quartile_gt_3d_quartile, total_leq_2d_quartile,
  gt_2d_quartile_leq_3d_quartile, gt_2d_quartile_gt_3d_quartile, total_gt_2d_quartile,
  total_leq_3d_quartile, total_gt_3d_quartile, total
), nrow = 3, byrow = TRUE)

# Assign column and row names
colnames(table_counts) <- c("<=2d quartile", ">2d quartile", "Total")
rownames(table_counts) <- c("<=3d quartile", ">3d quartile", "Total")

# Print the table
print(table_counts)
