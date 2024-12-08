
# Assignment: Statistical Structures in Data

# Name: Prince Himadri Mayank
# Roll No: 24BM6JP43



# DATASET 1: AIRQUALITY

# 1. Data Overview

# Load the airquality dataset
data(airquality)

# Check for missing or infinite values
sum(is.na(airquality))  # Check for missing values

# Check for infinite values
sum(!is.finite(airquality$Ozone))
sum(!is.finite(airquality$Solar.R))
sum(!is.finite(airquality$Wind))
sum(!is.finite(airquality$Temp))
sum(!is.finite(airquality$Month))
sum(!is.finite(airquality$Day))

# Clean the dataset: Remove rows with NA or infinite values
airquality_cleaned <- airquality

# Replace infinite values with NA in each column
airquality_cleaned[!is.finite(airquality_cleaned$Ozone), "Ozone"] <- NA
airquality_cleaned[!is.finite(airquality_cleaned$Solar.R), "Solar.R"] <- NA

# Remove rows with NA values
airquality_cleaned <- na.omit(airquality_cleaned)

# Check the structure of the cleaned dataset
str(airquality_cleaned)

# Check the number of observations and variables
n_obs1 <- nrow(airquality_cleaned)
n_vars1 <- ncol(airquality_cleaned)
cat("Number of observations:", n_obs1, "\n")
cat("Number of variables:", n_vars1, "\n")


# 2. Summary Statistics

# Choose the variable 'Ozone'
var1 <- airquality_cleaned$Ozone

# Calculate summary statistics
mean_var1 <- mean(var1)
median_var1 <- median(var1)
sd_var1 <- sd(var1)
min_var1 <- min(var1)
max_var1 <- max(var1)

cat("Mean:", mean_var1, "\n")
cat("Median:", median_var1, "\n")
cat("Standard Deviation:", sd_var1, "\n")
cat("Minimum:", min_var1, "\n")
cat("Maximum:", max_var1, "\n")


# 3. Distribution Visualization

# Histogram
hist(airquality_cleaned$Ozone, 
     main = "Histogram of Ozone", 
     xlab = "Ozone Concentration", 
     col = "lightblue", 
     border = "black", 
     breaks = 10)

# Boxplot
boxplot(airquality_cleaned$Ozone, 
        main = "Boxplot of Ozone", 
        xlab = "Ozone Concentration", 
        col = "lightgreen",
        horizontal = TRUE)


# 4. Categorical Variable Analysis

# Bar plot for 'Month'
barplot(table(airquality_cleaned$Month), 
        main = "Bar plot of Months", 
        xlab = "Month", 
        ylab = "Frequency", 
        col = "lightcoral")


# 5. Correlation Analysis

# Pearson correlation between 'Ozone' and 'Solar.R'
cor_ozone_solar <- cor(airquality_cleaned$Ozone, airquality_cleaned$Solar.R)
cat("Pearson Correlation between Ozone and Solar.R:", cor_ozone_solar, "\n")


# 6. Scatter Plot Visualization

# Scatter plot with trend line
plot(airquality_cleaned$Ozone, airquality_cleaned$Solar.R, 
     main = "Scatter plot of Ozone vs Solar.R", 
     xlab = "Ozone Concentration", 
     ylab = "Solar Radiation", 
     pch = 19, 
     col = "blue")
abline(lm(Solar.R ~ Ozone, data = airquality_cleaned), col = "red", lwd = 2)


# 7. Multiple Regression

# Fit the linear regression model
model1 <- lm(Ozone ~ Solar.R + Wind, data = airquality_cleaned)

# Display the model summary
summary(model1)


# 8. Model Diagnostics

# Plot residuals for the linear regression model
plot(model1)


# 9. Principal Component Analysis (PCA)

# Perform PCA
pca1 <- prcomp(airquality_cleaned[, c("Ozone", "Solar.R", "Wind", "Temp")], scale = TRUE)

# Plot the explained variance
summary(pca1)

# Get the proportion of variance explained by each principal component
explained_variance1 <- summary(pca1)$importance[2,]  # Proportion of Variance

# Create a data frame for plotting
pca_df1 <- data.frame(
  PC = factor(1:length(explained_variance1), levels = 1:length(explained_variance1)),
  Variance = explained_variance1
)

# Create a ggplot for the scree plot
library(ggplot2)
ggplot(pca_df1, aes(x = PC, y = Variance)) +
  geom_line(group = 1, color = "blue", size = 1.2) +  # Line plot
  geom_point(color = "red", size = 3) +  # Points on the line
  labs(
    title = "Scree Plot of PCA (airquality)",
    x = "Principal Component",
    y = "Proportion of Variance"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )


# 10. PCA Interpretation

# Biplot of PCA without point labels (numbers)
biplot(pca1, main = "Biplot of PCA (airquality)", cex = 0.5)




# DATASET 2: IRIS

# 1. Data Overview

# Load the iris dataset
data(iris)

# Check for missing or infinite values
sum(is.na(iris))  # Check for missing values

# Check for infinite values
sum(!is.finite(iris$Sepal.Length))
sum(!is.finite(iris$Sepal.Width))
sum(!is.finite(iris$Petal.Length))
sum(!is.finite(iris$Petal.Width))

# No missing or infinite values found in the iris dataset

# Check the structure of the iris dataset
str(iris)

# Get the number of observations and variables
n_obs2 <- nrow(iris)
n_vars2 <- ncol(iris)
cat("Number of observations:", n_obs2, "\n")
cat("Number of variables:", n_vars2, "\n")


# 2. Summary Statistics

# Choose the variable 'Sepal.Length'
var2 <- iris$Sepal.Length

# Calculate summary statistics
mean_var2 <- mean(var2)
median_var2 <- median(var2)
sd_var2 <- sd(var2)
min_var2 <- min(var2)
max_var2 <- max(var2)

cat("Mean:", mean_var2, "\n")
cat("Median:", median_var2, "\n")
cat("Standard Deviation:", sd_var2, "\n")
cat("Minimum:", min_var2, "\n")
cat("Maximum:", max_var2, "\n")


# 3. Distribution Visualization
# Histogram
hist(iris$Sepal.Length, 
     main = "Histogram of Sepal Length", 
     xlab = "Sepal Length", 
     col = "lightblue", 
     border = "black", 
     breaks = 10)

# Boxplot
boxplot(iris$Sepal.Length, 
        main = "Boxplot of Sepal Length", 
        xlab = "Sepal Length", 
        col = "lightgreen",
        horizontal = TRUE)


# 4. Categorical Variable Analysis

# Bar plot for 'Species'
barplot(table(iris$Species), 
        main = "Bar plot of Species", 
        xlab = "Species", 
        ylab = "Frequency", 
        col = "lightcoral")


# 5. Correlation Analysis

# Pearson correlation between 'Sepal.Length' and 'Petal.Length'
cor_sepal_petal <- cor(iris$Sepal.Length, iris$Petal.Length)
cat("Pearson Correlation between Sepal Length and Petal Length:", cor_sepal_petal, "\n")


# 6. Scatter Plot Visualization

# Scatter plot with trend line
plot(iris$Sepal.Length, iris$Petal.Length, 
     main = "Scatter plot of Sepal Length vs Petal Length", 
     xlab = "Sepal Length", 
     ylab = "Petal Length", 
     pch = 19, 
     col = "blue")
abline(lm(Petal.Length ~ Sepal.Length, data = iris), col = "red", lwd = 2)


# 7. Multiple Regression

# Fit the linear regression model
model2 <- lm(Sepal.Length ~ Petal.Length + Sepal.Width, data = iris)

# Display the model summary
summary(model2)


# 8. Model Diagnostics

# Plot residuals for the linear regression model
plot(model2)


# 9. Principal Component Analysis (PCA)

# Perform PCA
pca2 <- prcomp(iris[, 1:4], scale = TRUE)

# Plot the explained variance
summary(pca2)

# Get the proportion of variance explained by each principal component
explained_variance2 <- summary(pca2)$importance[2,]  # Proportion of Variance

# Create a data frame for plotting
pca_df2 <- data.frame(
  PC = factor(1:length(explained_variance2), levels = 1:length(explained_variance2)),
  Variance = explained_variance2
)

# Create a ggplot for the scree plot
library(ggplot2)
ggplot(pca_df2, aes(x = PC, y = Variance)) +
  geom_line(group = 1, color = "blue", size = 1.2) +  # Line plot
  geom_point(color = "red", size = 3) +  # Points on the line
  labs(
    title = "Scree Plot of PCA (iris)",
    x = "Principal Component",
    y = "Proportion of Variance"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )


# 10. PCA Interpretation

# Biplot of PCA
biplot(pca2, main = "Biplot of PCA (iris)", cex = 0.5)



# DATASET 3: MTCARS

# 1. Data Overview

# Load the mtcars dataset
data(mtcars)

# Check for missing values
sum(is.na(mtcars))  # Check for missing values

# Check for infinite values
sum(!is.finite(mtcars$mpg))
sum(!is.finite(mtcars$hp))
sum(!is.finite(mtcars$wt))
sum(!is.finite(mtcars$cyl))
sum(!is.finite(mtcars$disp))
sum(!is.finite(mtcars$drat))
sum(!is.finite(mtcars$qsec))
sum(!is.finite(mtcars$vs))
sum(!is.finite(mtcars$am))
sum(!is.finite(mtcars$gear))
sum(!is.finite(mtcars$carb))

# No missing or infinite values in the mtcars dataset

# Check the structure of the mtcars dataset
str(mtcars)

# Get the number of observations and variables
n_obs3 <- nrow(mtcars)
n_vars3 <- ncol(mtcars)
cat("Number of observations:", n_obs3, "\n")
cat("Number of variables:", n_vars3, "\n")


# 2. Summary Statistics

# Choose the variable 'mpg'
var3 <- mtcars$mpg

# Calculate summary statistics
mean_var3 <- mean(var3)
median_var3 <- median(var3)
sd_var3 <- sd(var3)
min_var3 <- min(var3)
max_var3 <- max(var3)

cat("Mean:", mean_var3, "\n")
cat("Median:", median_var3, "\n")
cat("Standard Deviation:", sd_var3, "\n")
cat("Minimum:", min_var3, "\n")
cat("Maximum:", max_var3, "\n")


# 3. Distribution Visualization

# Histogram
hist(mtcars$mpg, 
     main = "Histogram of MPG", 
     xlab = "Miles per Gallon", 
     col = "lightblue", 
     border = "black", 
     breaks = 10)

# Boxplot
boxplot(mtcars$mpg, 
        main = "Boxplot of MPG", 
        xlab = "Miles per Gallon", 
        col = "lightgreen",
        horizontal = TRUE)


# 4. Categorical Variable Analysis

# Bar plot for 'cyl'
barplot(table(mtcars$cyl), 
        main = "Bar plot of Cylinders", 
        xlab = "Number of Cylinders", 
        ylab = "Frequency", 
        col = "lightcoral")


# 5. Correlation Analysis

# Pearson correlation between 'mpg' and 'hp'
cor_mpg_hp <- cor(mtcars$mpg, mtcars$hp)
cat("Pearson Correlation between MPG and HP:", cor_mpg_hp, "\n")


# 6. Scatter Plot Visualization

# Scatter plot with trend line
plot(mtcars$mpg, mtcars$hp, 
     main = "Scatter plot of MPG vs HP", 
     xlab = "Miles per Gallon", 
     ylab = "Horsepower", 
     pch = 19, 
     col = "blue")
abline(lm(hp ~ mpg, data = mtcars), col = "red", lwd = 2)


# 7. Multiple Regression

# Fit the linear regression model
model3 <- lm(mpg ~ hp + wt, data = mtcars)

# Display the model summary
summary(model3)


# 8. Model Diagnostics

# Plot residuals for the linear regression model
plot(model3)


# 9. Principal Component Analysis (PCA)

# Perform PCA
pca3 <- prcomp(mtcars[, c("mpg", "hp", "wt", "qsec", "drat", "disp")], scale = TRUE)

# Plot the explained variance
summary(pca3)

# Get the proportion of variance explained by each principal component
explained_variance3 <- summary(pca3)$importance[2,]  # Proportion of Variance

# Create a data frame for plotting
pca_df3 <- data.frame(
  PC = factor(1:length(explained_variance3), levels = 1:length(explained_variance3)),
  Variance = explained_variance3
)

# Create a ggplot for the scree plot
library(ggplot2)
ggplot(pca_df3, aes(x = PC, y = Variance)) +
  geom_line(group = 1, color = "blue", size = 1.2) +  # Line plot
  geom_point(color = "red", size = 3) +  # Points on the line
  labs(
    title = "Scree Plot of PCA (mtcars)",
    x = "Principal Component",
    y = "Proportion of Variance"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )


# 10. PCA Interpretation

# Biplot of PCA
biplot(pca3, main = "Biplot of PCA (mtcars)", cex = 0.5)



# DATASET 4: LONGLEY

# 1. Data Overview

# Load the longley dataset
data(longley)

# Check for missing values
sum(is.na(longley))  # Check for missing values

# Check for infinite values in the dataset
sum(!is.finite(longley$GNP))
sum(!is.finite(longley$GNP.deflator))
sum(!is.finite(longley$Unemployed))
sum(!is.finite(longley$Population))
sum(!is.finite(longley$Armed.Forces))
sum(!is.finite(longley$Year))
sum(!is.finite(longley$Employed))

# No missing or infinite values found in the longley dataset

# Check the structure of the longley dataset
str(longley)

# Get the number of observations and variables
n_obs4 <- nrow(longley)
n_vars4 <- ncol(longley)
cat("Number of observations:", n_obs4, "\n")
cat("Number of variables:", n_vars4, "\n")


# 2. Summary Statistics

# Choose the variable 'GNP'
var4 <- longley$GNP

# Calculate summary statistics
mean_var4 <- mean(var4)
median_var4 <- median(var4)
sd_var4 <- sd(var4)
min_var4 <- min(var4)
max_var4 <- max(var4)

cat("Mean:", mean_var4, "\n")
cat("Median:", median_var4, "\n")
cat("Standard Deviation:", sd_var4, "\n")
cat("Minimum:", min_var4, "\n")
cat("Maximum:", max_var4, "\n")


# 3. Distribution Visualization

# Histogram for GNP
hist(longley$GNP, 
     main = "Histogram of GNP", 
     xlab = "GNP", 
     col = "lightblue", 
     border = "black", 
     breaks = 10)

# Boxplot for GNP
boxplot(longley$GNP, 
        main = "Boxplot of GNP", 
        xlab = "GNP", 
        col = "lightgreen",
        horizontal = TRUE)


# 4. Categorical Variable Analysis

# Convert 'Year' to a factor for visualization
longley$Year <- as.factor(longley$Year)

# Bar plot for 'Year'
barplot(table(longley$Year), 
        main = "Bar plot of Year", 
        xlab = "Year", 
        ylab = "Frequency", 
        col = "lightcoral")


# 5. Correlation Analysis

# Pearson correlation between 'GNP' and 'Unemployed'
cor_gnp_unemployed <- cor(longley$GNP, longley$Unemployed)
cat("Pearson Correlation between GNP and Unemployed:", cor_gnp_unemployed, "\n")


# 6. Scatter Plot Visualization

# Scatter plot with trend line
plot(longley$GNP, longley$Unemployed, 
     main = "Scatter plot of GNP vs Unemployed", 
     xlab = "GNP", 
     ylab = "Unemployed", 
     pch = 19, 
     col = "blue")
abline(lm(Unemployed ~ GNP, data = longley), col = "red", lwd = 2)


# 7. Multiple Regression

# Fit the linear regression model
model4 <- lm(GNP ~ Unemployed + Population, data = longley)

# Display the model summary
summary(model4)


# 8. Model Diagnostics

# Plot residuals for the linear regression model
plot(model4)


# 9. Principal Component Analysis (PCA)

# Select only the numeric columns for PCA (exclude the 'Year' column)
longley_numeric <- longley[, sapply(longley, is.numeric)]

# Perform PCA on the numeric columns
pca4 <- prcomp(longley_numeric, scale = TRUE)

# Plot the explained variance
summary(pca4)

# Get the proportion of variance explained by each principal component
explained_variance4 <- summary(pca4)$importance[2,]  # Proportion of Variance

# Create a data frame for plotting
pca_df4 <- data.frame(
  PC = factor(1:length(explained_variance4), levels = 1:length(explained_variance4)),
  Variance = explained_variance4
)

# Create a ggplot for the scree plot
library(ggplot2)
ggplot(pca_df4, aes(x = PC, y = Variance)) +
  geom_line(group = 1, color = "blue", size = 1.2) +  # Line plot
  geom_point(color = "red", size = 3) +  # Points on the line
  labs(
    title = "Scree Plot of PCA (longley)",
    x = "Principal Component",
    y = "Proportion of Variance"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )


# 10. PCA Interpretation

# Biplot of PCA
biplot(pca4, main = "Biplot of PCA (longley)", cex = 0.5)

