# Project Title: Ransomware Attack Analysis & Temporal Trends #

## Project Overview ##
This project focuses on analyzing historical ransomware attack data to uncover patterns in ransom demands. We aim to understand if the severity of an attack—measured by how long a system is unavailable (downtime)—directly correlates with the amount of money demanded by attackers. Additionally, we investigate whether these ransom demands have been increasing over the years.

## Key Objectives ##
- Q1: System Unavailability Impact: Investigate how different levels of system downtime (Short, Medium, Long) affect the average ransom amount.

- Q2: Temporal Trends: Analyze whether there has been a significant increase in average ransom demands over the recorded years.

## Technical Approach ##
- The analysis was conducted using R, utilizing several key libraries for data manipulation and visualization:

- Data Manipulation: dplyr, tidyverse, data.table

- Visualization: ggplot2, scales

- Statistical Analysis: stats (for Kruskal-Wallis and Wilcoxon tests to handle the non-normal distribution of ransom data).

## How to Run the Analysis ##
1. Ensure you have R and RStudio installed.

2. Clone this repository to your local machine.

3. Set the working directory in the R script to point to the AssignmentDatasets folder:

4. R: 
    setwd("YOUR_PATH_HERE/AssignmentDatasets")
    Run the provided analysis.R script to generate the statistical summaries, plots, and hypothesis test results.

## Key Findings ##
- Downtime Correlation: Our analysis indicates that longer system unavailability generally corresponds to higher ransom demands, suggesting that attackers leverage the urgency of prolonged downtime to extract larger payments.

- Yearly Trend: The visualization of annual trends identifies whether there is a sustained upward trajectory in the financial impact of these cyberattacks.

## Author ##
Leong Yuan Kang (TP087450)
Asia Pacific University of Technology & Innovation (APU)
