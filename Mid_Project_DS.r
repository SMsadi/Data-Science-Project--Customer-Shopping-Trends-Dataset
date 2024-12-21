library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)

file_path <- "G:/Data Science/Mid_Project/Midterm_Dataset_Section(A).xlsx"
df <- read_excel(file_path)

head(df)


missing_values <- sapply(df, function(x) sum(is.na(x)))
missing_values <- missing_values[missing_values > 0]

barplot(missing_values, main="Missing Values in Each Column", 
        xlab="Columns", ylab="Number of Missing Values", col="steelblue")


colSums(is.na(df))

#1
for (col in names(df)) 
  if (is.numeric(df[[col]])) 
    df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)



#2
for (col in names(df)) 
  if (is.numeric(df[[col]])) 
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)

#3
Mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

for (col in names(df)) {
  if (is.numeric(df[[col]])) {
    df[[col]][is.na(df[[col]])] <- floor(mean(df[[col]], na.rm = TRUE))
  } else {
    df[[col]][is.na(df[[col]])] <- Mode(df[[col]])
  }
}
#4
df <- na.omit(df)

#5
df <- df[, colSums(is.na(df)) == 0]

#Recheck
colSums(is.na(df))

missing_values <- sapply(df, function(x) sum(is.na(x)))
missing_values <- missing_values[missing_values > 0]

barplot(missing_values, main="Missing Values in Each Column", 
        xlab="Columns", ylab="Number of Missing Values", col="steelblue")

df <- distinct(df)

filtered_data <- df %>% filter(`Purchase Amount (USD)` > 50)

View(filtered_data)

df$Gender <- as.numeric(factor(df$Gender))

head(df$Gender)

df$AgeGroup <- cut(df$Age, 
                   breaks = c(0, 18, 30, 45, 60, Inf), 
                   labels = c("0-18", "19-30", "31-45", "46-60", "60+"),
                   right = FALSE)

head(df$AgeGroup)


df$Normalized_Purchase_Amount <- (df$`Purchase Amount (USD)` - min(df$`Purchase Amount (USD)`)) / 
  (max(df$`Purchase Amount (USD)`) - min(df$`Purchase Amount (USD)`))


Q1 <- quantile(df$`Review Rating`, 0.25)
Q3 <- quantile(df$`Review Rating`, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

df_cleaned <- df %>% filter(`Review Rating` >= lower_bound & `Review Rating` <= upper_bound)

descriptive_stats <- summary(df_cleaned)
write.csv(descriptive_stats, 'descriptive_statistics.csv')


write.csv(filtered_data, 'filtered_data.csv', row.names = FALSE)


getwd()

