# PSSA Science Assessment Analysis
# Pennsylvania State System of Assessment (2008-2024)
# Authors: Ritvik Kothapalyam & Amal Parekh


# Load Required Libraries
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam

library(tidyverse)
library(ggplot2)
library(knitr)

# Create DATASET
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam

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

# DATA CLEANING AND PREPARATION
# Primary Author: Ritvik Kothapalyam
# Reviewer: Amal Parekh

# Convert Data to percentage (multiply by 100)
pssa_data$Percentage <- pssa_data$Data * 100

# Set factor levels for Score to maintain proper order
pssa_data$Score <- factor(
  pssa_data$Score, 
  levels = c("Advanced", "Proficient", "Basic", "Below Basic")
)

# Create a year variable for easier plotting
pssa_data$Year <- as.numeric(substr(pssa_data$TimeFrame, 1, 4))

# Display first few rows
cat("First 10 rows of cleaned PSSA data:\n")
print(head(pssa_data, 10))

# CALCULATE SUMMARY STATISTICS
# Primary Author: Ritvik Kothapalyam
# Reviewer: Amal Parekh

summary_stats <- pssa_data %>%
  group_by(Score) %>%
  summarize(
    Mean = round(mean(Percentage), 1),
    Min = round(min(Percentage), 1),
    Max = round(max(Percentage), 1),
    SD = round(sd(Percentage), 1),
    .groups = "drop"
  )

cat("\nSummary Statistics by Proficiency Level:\n")
print(summary_stats)

# CALCULATE COMBINED PROFICIENCY RATES
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam

proficiency_rates <- pssa_data %>%
  filter(Score %in% c("Advanced", "Proficient")) %>%
  group_by(TimeFrame, Year) %>%
  summarize(Combined_Proficiency = sum(Percentage), .groups = "drop")

cat("\nCombined Proficiency Rates (Advanced + Proficient):\n")
print(proficiency_rates)

# VISUALIZATION 1: LINE CHART - TRENDS OVER TIME
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam

graph1 <- ggplot(pssa_data, aes(x = Year, y = Percentage, 
                                color = Score, group = Score)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Advanced" = "#2E7D32", 
      "Proficient" = "#1976D2",
      "Basic" = "#F57C00",
      "Below Basic" = "#C62828"
    )
  ) +
  labs(
    title = "Pennsylvania PSSA Science Assessment Trends (2008-2024)",
    x = "School Year",
    y = "Percentage of Students (%)",
    color = "Proficiency Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(2008, 2023, 2))

print(graph1)

# VISUALIZATION 2: STACKED AREA CHART
# Primary Author: Ritvik Kothapalyam
# Reviewer: Amal Parekh

graph2 <- ggplot(pssa_data, aes(x = Year, y = Percentage, fill = Score)) +
  geom_area(alpha = 0.7) +
  scale_fill_manual(
    values = c(
      "Advanced" = "#2E7D32", 
      "Proficient" = "#1976D2",
      "Basic" = "#F57C00",
      "Below Basic" = "#C62828"
    )
  ) +
  labs(
    title = "PSSA Science Proficiency Distribution (Stacked)",
    x = "School Year",
    y = "Percentage of Students (%)",
    fill = "Proficiency Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

print(graph2)

# VISUALIZATION 3: COMBINED PROFICIENCY RATE
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam

graph3 <- ggplot(proficiency_rates, aes(x = Year, y = Combined_Proficiency)) +
  geom_line(color = "#1976D2", linewidth = 1.5) +
  geom_point(color = "#1976D2", size = 4) +
  geom_hline(
    yintercept = mean(proficiency_rates$Combined_Proficiency), 
    linetype = "dashed", 
    color = "red", 
    linewidth = 1
  ) +
  labs(
    title = "Combined Proficiency Rate (Advanced + Proficient)",
    x = "School Year",
    y = "Combined Proficiency Rate (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")) +
  scale_x_continuous(breaks = seq(2008, 2023, 2)) +
  ylim(55, 75)

print(graph3)

# VISUALIZATION 4: GROUPED BAR CHART
# Primary Author: Ritvik Kothapalyam
# Reviewer: Amal Parekh

graph4 <- ggplot(pssa_data, aes(x = factor(Year), y = Percentage, 
                                fill = Score)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_manual(
    values = c(
      "Advanced" = "#2E7D32", 
      "Proficient" = "#1976D2",
      "Basic" = "#F57C00",
      "Below Basic" = "#C62828"
    )
  ) +
  labs(
    title = "Year-by-Year Comparison of PSSA Science Proficiency",
    x = "School Year",
    y = "Percentage of Students (%)",
    fill = "Proficiency Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

print(graph4)

# SAVE PLOTS TO FILE
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam

# Set output directory
output_dir <- "/Users/amalparekh/Downloads"

# Save all graphs as PNG files
ggsave(
  filename = file.path(output_dir, "pssa_graph1_line_trends.png"),
  plot = graph1,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "pssa_graph2_stacked_area.png"),
  plot = graph2,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "pssa_graph3_combined_proficiency.png"),
  plot = graph3,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "pssa_graph4_grouped_bars.png"),
  plot = graph4,
  width = 12,
  height = 6,
  dpi = 300
)

cat("\nAll plots saved to:", output_dir, "\n")

# KEY FINDINGS SUMMARY
cat("\n==================================================\n")
cat("KEY FINDINGS FROM PSSA SCIENCE ANALYSIS\n")
cat("==================================================\n")
cat("1. Overall Improvement: Combined proficiency increased from 59.1% (2008-09)\n")
cat("   to 65.8% (2023-24)\n")
cat("2. Assessment Change Impact: Sharp increase in proficiency in 2012-13\n")
cat("3. Pre-Pandemic Peak: Highest proficiency (68.0%) achieved in 2018-19\n")
cat("4. Pandemic Impact: Drop to 62.2% proficiency in 2021-22\n")
cat("5. Recovery Trajectory: Proficiency improving in recent years (2022-24)\n")
cat("6. Below Basic Reduction: Decreased from 16.4% to 16.1% over 15 years\n")
cat("==================================================\n")

cat("\nAnalysis complete! All visualizations created and saved.\n")
