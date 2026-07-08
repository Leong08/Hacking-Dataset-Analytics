library(dplyr)
library(readxl)
library(curl)
library(stringr)
library(tidyverse)
library(data.table)

options(scipen = 999)
setwd("C:/Users/leong/Desktop/APU - Degree Y2/Programming for Data Analysis/Assignment/AssignmentDatasets")

df <- read_csv("HackingData_Cleaned.csv",  show_col_types = FALSE)

view(df)

df <- df %>%
  select(Ransom, Notify, Encoding, WebServer, Country, DownTime) %>%
  mutate(
    Ransom = expm1(Ransom),
    DownTime = expm1(DownTime)
  )


par(mfrow=c(1,2)) # Used to show QQ plot and histogram at the same time

set.seed(123) # For reproducibility
group_shapiro_results <- shapiro.test(sample(df$Ransom, 5000))

print(group_shapiro_results)


# Generate QQ Plot for visualization
qqnorm(df$Ransom, main = "Normality QQ Plot")
qqline(df$Ransom, col = "red")


# Generate Histogram to check the is that any bell curve
hist(df$Ransom, 
     main = "Histogram of Ransom", 
     xlab = "Ransom Amount", 
     col = "lightblue", 
     border = "white")


# Kruskal-Wallis Test Method
test_notify <- kruskal.test(Ransom ~ Notify, data = df)
test_encoding <- kruskal.test(Ransom ~ Encoding, data = df)
test_server <- kruskal.test(Ransom ~ WebServer, data = df)
test_country <- kruskal.test(Ransom ~ Country, data = df)
test_downtime <- kruskal.test(Ransom ~ DownTime, data = df)

# Summary Table
group_results <- data.frame(
  Variable = c("Notify", "Encoding", "WebServer", "Country", "DownTime"),
  Chi_Squared = c(test_notify$statistic, test_encoding$statistic, test_server$statistic, 
                  test_country$statistic, test_downtime$statistic),
  P_Value = c(test_notify$p.value, test_encoding$p.value, test_server$p.value, 
              test_country$p.value, test_downtime$p.value)
)

view(group_results)

