# PSSA Science Assessment Analysis
# Pennsylvania State System of Assessment (2008-2024)
# Authors: Ritvik Kothapalyam & Amal Parekh

# Load required libraries
library(tidyverse)
library(ggplot2)

# Create the dataset manually from the PDF data
pssa_data <- data.frame(
  LocationType = rep("State", 52),
  Location = rep("Pennsylvania", 52),
  Score = c(
    # 2008-09
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2009-10
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2010-11
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2011-12
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2012-13
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2014-15
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2015-16
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2016-17
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2017-18
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2018-19
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2021-22
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2022-23
    "Advanced", "Proficient", "Basic", "Below Basic",
    # 2023-24
    "Advanced", "Proficient", "Basic", "Below Basic"
  ),
  TimeFrame = c(
    rep("2008-09", 4), rep("2009-10", 4), rep("2010-11", 4), rep("2011-12", 4),
    rep("2012-13", 4), rep("2014-15", 4), rep("2015-16", 4), rep("2016-17", 4),
    rep("2017-18", 4), rep("2018-19", 4), rep("2021-22", 4), rep("2022-23", 4),
    rep("2023-24", 4)
  ),
  DataFormat = rep("Percent", 52),
  Data = c(
    # 2008-09
    0.272, 0.319, 0.245, 0.164,
    # 2009-10
    0.280, 0.314, 0.231, 0.175,
    # 2010-11
    0.279, 0.330, 0.235, 0.157,
    # 2011-12
    0.278, 0.337, 0.241, 0.144,
    # 2012-13
    0.34487, 0.34694, 0.14985, 0.15834,
    # 2014-15
    0.339, 0.339, 0.152, 0.170,
    # 2015-16
    0.33418, 0.3356, 0.14443, 0.18529,
    # 2016-17
    0.271, 0.366, 0.213, 0.150,
    # 2017-18
    0.281, 0.366, 0.215, 0.138,
    # 2018-19
    0.309, 0.371, 0.195, 0.125,
    # 2021-22
    0.285, 0.337, 0.195, 0.183,
    # 2022-23
    0.307, 0.349, 0.182, 0.163,
    # 2023-24
    0.295, 0.363, 0.181, 0.161
  )
)

# Convert Data to percentage (multiply by 100)
pssa_data$Percentage <- pssa_data$Data * 100

# Set factor levels for Score to maintain proper order
pssa_data$Score <- factor(pssa_data$Score, 
                          levels = c("Advanced", "Proficient", "Basic", "Below Basic"))

# Create a year variable for easier plotting
pssa_data$Year <- as.numeric(substr(pssa_data$TimeFrame, 1, 4))

# Display the cleaned data
print("PSSA Science Assessment Data (2008-2024)")
print(head(pssa_data, 10))

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
summary_stats <- pssa_data %>%
  group_by(Score) %>%
  summarize(
    Mean_Percentage = mean(Percentage),
    Min_Percentage = min(Percentage),
    Max_Percentage = max(Percentage),
    SD_Percentage = sd(Percentage)
  )
print(summary_stats)

# Calculate combined proficiency rates (Advanced + Proficient)
proficiency_rates <- pssa_data %>%
  filter(Score %in% c("Advanced", "Proficient")) %>%
  group_by(TimeFrame, Year) %>%
  summarize(Combined_Proficiency = sum(Percentage), .groups = "drop")

cat("\n=== COMBINED PROFICIENCY RATES (Advanced + Proficient) ===\n")
print(proficiency_rates)

# GRAPH 1: Line Chart showing trends over time
print("Creating Graph 1: Line Chart of All Proficiency Levels...")
ggplot(pssa_data, aes(x = Year, y = Percentage, color = Score, group = Score)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Advanced" = "#2E7D32", 
                                "Proficient" = "#1976D2",
                                "Basic" = "#F57C00",
                                "Below Basic" = "#C62828")) +
  labs(
    title = "Pennsylvania PSSA Science Assessment Trends (2008-2024)",
    subtitle = "Distribution of Student Proficiency Levels Over Time",
    x = "School Year",
    y = "Percentage of Students (%)",
    color = "Proficiency Level",
    caption = "Source: Pennsylvania State System of Assessment (PSSA)\nNote: No data available for 2013-14 and 2019-21 (COVID-19 period)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(2008, 2023, 2))

# GRAPH 2: Stacked Area Chart
print("Creating Graph 2: Stacked Area Chart...")
ggplot(pssa_data, aes(x = Year, y = Percentage, fill = Score)) +
  geom_area(alpha = 0.7) +
  scale_fill_manual(values = c("Advanced" = "#2E7D32", 
                               "Proficient" = "#1976D2",
                               "Basic" = "#F57C00",
                               "Below Basic" = "#C62828")) +
  labs(
    title = "PSSA Science Proficiency Distribution (Stacked)",
    subtitle = "Total Distribution Across All Proficiency Levels",
    x = "School Year",
    y = "Percentage of Students (%)",
    fill = "Proficiency Level"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))

# GRAPH 3: Combined Proficiency Rate Chart
print("Creating Graph 3: Combined Proficiency Rate...")
ggplot(proficiency_rates, aes(x = Year, y = Combined_Proficiency)) +
  geom_line(color = "#1976D2", size = 1.5) +
  geom_point(color = "#1976D2", size = 4) +
  geom_hline(yintercept = mean(proficiency_rates$Combined_Proficiency), 
             linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Combined Proficiency Rate Trends (Advanced + Proficient)",
    subtitle = "Percentage of Students Meeting or Exceeding Proficiency Standards",
    x = "School Year",
    y = "Combined Proficiency Rate (%)",
    caption = "Red dashed line indicates average combined proficiency rate"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  scale_x_continuous(breaks = seq(2008, 2023, 2)) +
  ylim(55, 75)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Three graphs have been created!\n")
cat("Look at the 'Plots' pane (bottom-right) to view them.\n")
cat("Use the arrow buttons to navigate between graphs.\n")
cat("Click 'Export' to save graphs as images.\n")






## COPY THIS CODE INTO CONSOLE TO PRODUCE GRAPHS
# Convert Data to percentage (multiply by 100)
pssa_data$Percentage <- pssa_data$Data * 100

# Set factor levels for Score to maintain proper order
pssa_data$Score <- factor(pssa_data$Score, 
                          levels = c("Advanced", "Proficient", "Basic", "Below Basic"))

# Create a year variable for easier plotting
pssa_data$Year <- as.numeric(substr(pssa_data$TimeFrame, 1, 4))

# GRAPH 1: Line Chart showing trends over time
ggplot(pssa_data, aes(x = Year, y = Percentage, color = Score, group = Score)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Advanced" = "#2E7D32", 
                                 "Proficient" = "#1976D2",
                                 "Basic" = "#F57C00",
                                 "Below Basic" = "#C62828")) +
  labs(
    title = "Pennsylvania PSSA Science Assessment Trends (2008-2024)",
    subtitle = "Distribution of Student Proficiency Levels Over Time",
    x = "School Year",
    y = "Percentage of Students (%)",
    color = "Proficiency Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )
