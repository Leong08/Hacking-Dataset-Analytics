
########################################################################################################################
# LEONG YUAN KANG TP087450
########################################################################################################################

library(dplyr)
library(readxl)
library(curl)
library(stringr)
library(tidyverse)
library(data.table)

options(scipen = 999)
setwd("C:/Users/leong/Desktop/APU - Degree Y2/Sem 1/Programming for Data Analysis/Assignment/AssignmentDatasets")
df <- read_csv("HackingData_Cleaned.csv",  show_col_types = FALSE)


# Objective : To investigate the effect of system unavailability duration and temporal trends on the Ransom amount to assess if attack severity drives higher payments

# Analysis Question
# Q1: How does the average Ransom amount vary across different levels of system unavailability
# Q2: Has the average Ransom demand increase significantly over the years? 

# Self defined function for statistical summary
cat_summary <- function(x) {
  freq_table <- table(x, useNA = "ifany")
  prop_table <- prop.table(freq_table)
  perc_table <- prop_table * 100
  mode_val <- names(freq_table)[which.max(freq_table)]
  
  #Combine into a Data Frame
  summary_df <- data.frame(
    Category = names(freq_table),
    Frequency = as.numeric(freq_table),
    Proportion = as.numeric(prop_table),
    Percentage = as.numeric(perc_table)
  )
  
  # Sort by Frequency
  summary_df <- summary_df[order(-summary_df$Frequency), ]
  
  return(summary_df)
}


num_summary <- function(x) {
  get_mode <- function(v) {
    uniqv <- unique(v[!is.na(v)])
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  x_clean <- x[!is.na(x)]
  q <- quantile(x_clean, probs = c(0.25, 0.75))
  med <- median(x_clean)
  mad_val <- mad(x_clean, constant = 1)
  res <- c(
    Min    = min(x_clean),
    Max    = max(x_clean),
    Q1     = q[1],
    Q3     = q[2],
    IQR    = IQR(x_clean),
    Mean   = mean(x_clean),
    Median = med,
    Mode   = get_mode(x_clean),
    MAD    = mad_val,
    RCV    = mad_val / med
  )
  
  return(res)
}


# Data Preparation
df_eda <- df %>%
  select(Date, DownTime, Ransom) %>%
  filter(!is.na(Date), !is.na(DownTime), !is.na(Ransom)) %>%
  mutate(
    Ransom_Raw = expm1(Ransom),
    DownTime_Raw = expm1(DownTime),
    Year = year(as.Date(Date)),
    DownTime_Level = cut(DownTime_Raw, 
                         breaks = c(-Inf, 1, 7, Inf), 
                         labels = c("Short (<1 Day)", "Medium (1-7 Days)", "Long (>7 Days)"))
  )

# Statistical Data
message("Statistical Summary: Real Ransom Amount")
print(num_summary(df_eda$Ransom_Raw))

message("Statistical Summary: Real DownTime (Days)")
print(num_summary(df_eda$DownTime_Raw))

message("Frequency of DownTime Severity")
print(cat_summary(df_eda$DownTime_Level))


# Analysis Question 1

# Grouped DownTime_Level Summary
downtime_summary <- df_eda %>%
  group_by(DownTime_Level) %>%
  summarise(
    Avg_Ransom_Real = mean(Ransom_Raw, na.rm = TRUE),
    Median_Ransom_Real = median(Ransom_Raw, na.rm = TRUE),
    Count = n()
  )
print(downtime_summary)

# Visualization for downtime_summary
downtime_long <- downtime_summary %>%
  pivot_longer(cols = c(Avg_Ransom_Real, Median_Ransom_Real), 
               names_to = "Metric", 
               values_to = "Value")

ggplot(downtime_long, aes(x = DownTime_Level, y = Value, fill = Metric)) +
  # "position_dodge" puts bars side-by-side
  geom_col(position = position_dodge(width = 0.9)) + 
  # Add labels
  geom_text(aes(label = paste0(scales::comma(round(Value, 0)))), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            fontface = "bold",
            size = 3.5) +
  labs(title = "Mean vs Median Ransom Amount by Downtime Severity",
       x = "DownTime Level",
       y = "Ransom Amount in Thounsand($)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1300)) + 
  scale_fill_manual(values = c("Avg_Ransom_Real" = "#4e79a7", "Median_Ransom_Real" = "#f28e2b"),
                    labels = c("Mean", "Median"))


# Analysis Question 2
yearly_trend <- df_eda %>%
  group_by(Year) %>%
  summarise(
    Avg_Ransom = mean(Ransom_Raw, na.rm = TRUE),
    Total_Attacks = n()
  ) %>%
  arrange(Year)
print(yearly_trend)

ggplot(yearly_trend, aes(x = Year, y = Avg_Ransom)) +
  # A professional Steel Blue for the line
  geom_line(color = "#4682B4", linewidth = 1.2) +
  # A bright Coral for the points to make them stand out
  geom_point(color = "#FF7F50", size = 3) +
  # Labels with the same Coral color to maintain consistency
  geom_text(aes(label = paste0(comma(round(Avg_Ransom, 0)))), 
            angle = 55,
            vjust = -0.3,
            hjust = -0.7,
            size = 3.5, 
            fontface = "bold",
            color = "#D35400",
            check_overlap = TRUE) + 
  labs(
    title = "Annual Trend of Average Ransom Demand",
    x = "Year",
    y = "Average Ransom in Thousand ($)"
  ) +
  theme_minimal() +
  # Expand Y-axis to make room for labels
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.1, 0.2))) +
  scale_x_continuous(breaks = pretty_breaks()) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#2C3E50"),
    panel.grid.minor = element_blank()
  )


# Hypothesis Testing

# Normality Check
set.seed(123) # For reproducibility
shapiro_results <- shapiro.test(sample(df_eda$Ransom_Raw, 5000))

print(shapiro_results)

# QQ Plot for visual check
qqnorm(df_eda$Ransom_Raw,
       main = "Normal Q-Q Plot: Ransom Amount Distribution",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Qunatiles (Ransom Raw)")
qqline(df_eda$Ransom_Raw, col = "red")

# Kruskal-Wallis Test
kruskal_result <- kruskal.test(Ransom_Raw ~ DownTime_Level, data = df_eda)
print(kruskal_result)
pairwise.wilcox.test(df_eda$Ransom_Raw, df_eda$DownTime_Level, p.adjust.method = "bonferroni")