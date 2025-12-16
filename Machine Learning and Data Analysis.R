#Importing all relevant Packages
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(caTools)
library(caret)
library(randomForest)

# Loading the Data
fileUrl = "C:\\Users\\ahmed\\Downloads\\UNSW-NB15_uncleaned.csv"
df = read.csv(fileUrl)

# Doing data cleaning for the label column
df$label = as.character(df$label)

# Removing the ? and _ from the data
df$label = str_replace_all(df$label, "[_?]", "")
df$label = str_replace_all(df$label, "[^0-9]", "")

# Convert to data to numerical
df$label = as.numeric(df$label)

# Using Mode to imputate any values that contain NA
mode_label = as.numeric(names(sort(table(df$label), decreasing = TRUE))[1])
df$label[is.na(df$label)] = mode_label

# Convert the label to a factor
df$label = factor(df$label, levels = c(0,1))

# Function for cleaning sinpkt & dinpkt
clean_column = function(col_vector) {
  col_vector = as.character(col_vector)
  
  # Removing any ? and _ from the data
  col_vector = str_replace_all(col_vector, "(NA_|NA\\?|NA)", NA_character_)
  col_vector = str_replace_all(col_vector, "[_?]", "")
  
  # Removing any non-numeric symbols
  col_vector = str_replace_all(col_vector, "[^0-9.]", "")
  col_vector = as.numeric(col_vector)
  
  return(col_vector)
}

# Applying the Cleaning
variables = c("sinpkt", "dinpkt", "sload", "dload", "spkts", "dpkts")

df_clean = df %>% mutate(
  across(all_of(variables), clean_column)
)

# Median imputation for all the variables
df_final = df_clean

for (v in variables) {
  med = median(df_final[[v]], na.rm = TRUE)
  df_final[[v]] = replace_na(df_final[[v]], med)
}

# Add cleaned label to final dataset
df_final$label = df$label

# Checking the median values used for imputation
summary(df_final[, variables])

# Checking the cleaned dataset
View(df_final)

# Making the point plot for sinpkt and dinpkt
ggplot(df_final, aes(x = sinpkt, y = dinpkt)) +
  geom_point(alpha = 0.5, size = 0.7, color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Scatter Plot of sinpkt vs dinpkt",
    x = "sinpkt (Source Inter-Packet Time)",
    y = "dinpkt (Destination Inter-Packet Time)"
  )

# Extra Feature 1 - Correlation Heatmap 
#-------------------------------------------------------------------------------
# Convert label to numeric for correlation
df_heat = df_final
df_heat$label = as.numeric(as.character(df_heat$label))

# Select columns for heatmap
heat_data = df_heat[, c("sinpkt", "dinpkt", "label")]

# Compute correlation matrix
corr_matrix = cor(heat_data)

# Display heatmap
corr_df = as.data.frame(as.table(corr_matrix))

ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "red", high = "darkgreen", mid = "yellow",
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap of sinpkt, dinpkt and label",
       x = "", y = "")
#-------------------------------------------------------------------------------
# Checking the datatypes for each column
str(df_final[, c("sinpkt", "dinpkt", "label", "sload", "dload", "spkts", "dpkts")])

# Extra Feature 2 - Logistic Regression
#-------------------------------------------------------------------------------
# Select ML columns
ml_data = df_final[, c("sinpkt", "dinpkt", "label")]
set.seed(123)

# Doing a 70-30 train-test split
split = sample.split(ml_data$label, SplitRatio = 0.7)
training_set = subset(ml_data, split == TRUE)
test_set = subset(ml_data, split == FALSE)

# Building a logistic regression model
model = glm(label ~ ., data = training_set, family = binomial)
summary(model)

# Predicting probabilities
pred_prob = predict(model, type = "response", newdata = test_set[, -3])

# Converting to a class prediction
pred_class = ifelse(pred_prob > 0.5, 1, 0)
pred_class = factor(pred_class, levels = c(0,1))

# Confusion Matrix
confusionMatrix(table(pred_class, test_set$label))

# Correct accuracy calculation
valid = !is.na(pred_class) & !is.na(test_set$label)
accuracy = sum(pred_class[valid] == test_set$label[valid]) / sum(valid)
accuracy
#-------------------------------------------------------------------------------

# Logistic Regression with more variables.

ml_data2 = df_final[, c("sinpkt", "dinpkt", "label", "sload", "dload", "spkts", "dpkts")]
set.seed(123)

# Doing a 70-30 train-test split
split2 = sample.split(ml_data2$label, SplitRatio = 0.7)
training_set2 = subset(ml_data2, split == TRUE)
test_set2 = subset(ml_data2, split == FALSE)

# Building a logistic regression model
model2 = glm(label ~ ., data = training_set2, family = binomial)
summary(model2)

# Predicting probabilities
pred_prob2 = predict(model2, type = "response", newdata = test_set2[, -3])

# Converting to a class prediction
pred_class2 = ifelse(pred_prob2 > 0.5, 1, 0)
pred_class2 = factor(pred_class2, levels = c(0,1))

# Confusion Matrix
confusionMatrix(table(pred_class2, test_set2$label))

# Correct accuracy calculation
valid2 = !is.na(pred_class2) & !is.na(test_set2$label)
accuracy2 = sum(pred_class2[valid2] == test_set2$label[valid2]) / sum(valid2)
accuracy2

#-------------------------------------------------------------------------------
# Extra Feature 3 - Random Forest Model

set.seed(123)
ml_data_rf = df_final[, c("sinpkt", "dinpkt", "sload", "dload", "spkts", "dpkts", "label")]

# Spilting the data
split_rf = sample.split(ml_data_rf$label, SplitRatio= 0.7)
train_rf = subset(ml_data_rf, split_rf == TRUE)
test_rf = subset(ml_data_rf, split_rf == FALSE)

# Building the model
rf_model = randomForest(
  label ~ sinpkt + dinpkt + sload + dload + spkts + dpkts,
  data = train_rf,
  ntree = 200,
  mtry = 3,
)

# Printing the summary
rf_model

# Prediction
rf_pred = predict(rf_model, newdata = test_rf)

# Confusion Matrix
confusionMatrix(table(rf_pred, test_rf$label))

# Accuracy
rf_accuracy = sum(rf_pred == test_rf$label) / nrow(test_rf)
rf_accuracy

#-------------------------------------------------------------------------------

# Data Validation

# Checking the number or rows and columns for Both datasets to insure no values we removed.
dim(df)
dim(df_final)

# Checking the overall values for both columns in the cleaned dataset.
summary(df_final$sinpkt)
summary(df_final$dinpkt)
summary(df_final$sload)
summary(df_final$dload)
summary(df_final$spkts)
summary(df_final$dpkts)
summary(df_final$label)

# Checking for any NA values in the cleaned Data
sum(is.na(df_final$sinpkt))
sum(is.na(df_final$dinpkt))
sum(is.na(df_final$label))
sum(is.na(df_final$sload))
sum(is.na(df_final$dload))
sum(is.na(df_final$spkts))
sum(is.na(df_final$dpkts))
