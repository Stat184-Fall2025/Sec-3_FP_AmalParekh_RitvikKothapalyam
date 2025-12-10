# ==============================================================================
# PSSA Science Score Distribution Analysis
# ==============================================================================
# Code Style Guide: Tidyverse Style Guide
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam
# ==============================================================================

# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)
library(ggridges)

# ==============================================================================
# DATA IMPORT AND CLEANING
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam
# ==============================================================================

# Read the PSSA data (manually created from PDF)
pssa_data <- tribble(
  ~location_type, ~location, ~score_level, ~time_frame, ~data_format, ~data,
  "State", "Pennsylvania", "Advanced", "2008-09", "Percent", 0.272,
  "State", "Pennsylvania", "Proficient", "2008-09", "Percent", 0.319,
  "State", "Pennsylvania", "Basic", "2008-09", "Percent", 0.245,
  "State", "Pennsylvania", "Below Basic", "2008-09", "Percent", 0.164,
  "State", "Pennsylvania", "Advanced", "2009-10", "Percent", 0.28,
  "State", "Pennsylvania", "Proficient", "2009-10", "Percent", 0.314,
  "State", "Pennsylvania", "Basic", "2009-10", "Percent", 0.231,
  "State", "Pennsylvania", "Below Basic", "2009-10", "Percent", 0.175,
  "State", "Pennsylvania", "Advanced", "2010-11", "Percent", 0.279,
  "State", "Pennsylvania", "Proficient", "2010-11", "Percent", 0.33,
  "State", "Pennsylvania", "Basic", "2010-11", "Percent", 0.235,
  "State", "Pennsylvania", "Below Basic", "2010-11", "Percent", 0.157,
  "State", "Pennsylvania", "Advanced", "2011-12", "Percent", 0.278,
  "State", "Pennsylvania", "Proficient", "2011-12", "Percent", 0.337,
  "State", "Pennsylvania", "Basic", "2011-12", "Percent", 0.241,
  "State", "Pennsylvania", "Below Basic", "2011-12", "Percent", 0.144,
  "State", "Pennsylvania", "Advanced", "2012-13", "Percent", 0.34487,
  "State", "Pennsylvania", "Proficient", "2012-13", "Percent", 0.34694,
  "State", "Pennsylvania", "Basic", "2012-13", "Percent", 0.14985,
  "State", "Pennsylvania", "Below Basic", "2012-13", "Percent", 0.15834,
  "State", "Pennsylvania", "Advanced", "2014-15", "Percent", 0.339,
  "State", "Pennsylvania", "Proficient", "2014-15", "Percent", 0.339,
  "State", "Pennsylvania", "Basic", "2014-15", "Percent", 0.152,
  "State", "Pennsylvania", "Below Basic", "2014-15", "Percent", 0.17,
  "State", "Pennsylvania", "Advanced", "2015-16", "Percent", 0.33418,
  "State", "Pennsylvania", "Proficient", "2015-16", "Percent", 0.3356,
  "State", "Pennsylvania", "Basic", "2015-16", "Percent", 0.14443,
  "State", "Pennsylvania", "Below Basic", "2015-16", "Percent", 0.18529,
  "State", "Pennsylvania", "Advanced", "2016-17", "Percent", 0.271,
  "State", "Pennsylvania", "Proficient", "2016-17", "Percent", 0.366,
  "State", "Pennsylvania", "Basic", "2016-17", "Percent", 0.213,
  "State", "Pennsylvania", "Below Basic", "2016-17", "Percent", 0.15,
  "State", "Pennsylvania", "Advanced", "2017-18", "Percent", 0.281,
  "State", "Pennsylvania", "Proficient", "2017-18", "Percent", 0.366,
  "State", "Pennsylvania", "Basic", "2017-18", "Percent", 0.215,
  "State", "Pennsylvania", "Below Basic", "2017-18", "Percent", 0.138,
  "State", "Pennsylvania", "Advanced", "2018-19", "Percent", 0.309,
  "State", "Pennsylvania", "Proficient", "2018-19", "Percent", 0.371,
  "State", "Pennsylvania", "Basic", "2018-19", "Percent", 0.195,
  "State", "Pennsylvania", "Below Basic", "2018-19", "Percent", 0.125,
  "State", "Pennsylvania", "Advanced", "2021-22", "Percent", 0.285,
  "State", "Pennsylvania", "Proficient", "2021-22", "Percent", 0.337,
  "State", "Pennsylvania", "Basic", "2021-22", "Percent", 0.195,
  "State", "Pennsylvania", "Below Basic", "2021-22", "Percent", 0.183,
  "State", "Pennsylvania", "Advanced", "2022-23", "Percent", 0.307,
  "State", "Pennsylvania", "Proficient", "2022-23", "Percent", 0.349,
  "State", "Pennsylvania", "Basic", "2022-23", "Percent", 0.182,
  "State", "Pennsylvania", "Below Basic", "2022-23", "Percent", 0.163,
  "State", "Pennsylvania", "Advanced", "2023-24", "Percent", 0.295,
  "State", "Pennsylvania", "Proficient", "2023-24", "Percent", 0.363,
  "State", "Pennsylvania", "Basic", "2023-24", "Percent", 0.181,
  "State", "Pennsylvania", "Below Basic", "2023-24", "Percent", 0.161
)

# Convert percentages to actual percentages (multiply by 100)
pssa_data <- pssa_data %>%
  mutate(
    percentage = data * 100,
    year = as.numeric(str_sub(time_frame, 1, 4))
  )

# Set factor levels for proper ordering
pssa_data$score_level <- factor(
  pssa_data$score_level,
  levels = c("Advanced", "Proficient", "Basic", "Below Basic")
)

# ==============================================================================
# EXPLORATORY DATA ANALYSIS
# Primary Author: Ritvik Kothapalyam
# Reviewer: Amal Parekh
# ==============================================================================

# Calculate combined proficiency rates
proficiency_summary <- pssa_data %>%
  filter(score_level %in% c("Advanced", "Proficient")) %>%
  group_by(time_frame, year) %>%
  summarise(
    combined_proficient = sum(percentage),
    .groups = "drop"
  )

# Print summary statistics
cat("\n=== PSSA Science Score Summary Statistics ===\n")
print(summary(pssa_data$percentage))

cat("\n=== Combined Proficiency Rates by Year ===\n")
print(proficiency_summary)

# ==============================================================================
# VISUALIZATION 1: Stacked Area Chart
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam
# ==============================================================================

plot1 <- ggplot(pssa_data, aes(x = year, y = percentage, fill = score_level)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(
    values = c(
      "Advanced" = "#2E7D32",
      "Proficient" = "#66BB6A",
      "Basic" = "#FFA726",
      "Below Basic" = "#E53935"
    ),
    name = "Proficiency Level"
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, 100)
  ) +
  labs(
    title = "Pennsylvania PSSA Science Score Distribution Over Time",
    subtitle = "Stacked distribution showing all proficiency levels (2008-2024)",
    x = "School Year",
    y = "Percentage of Students",
    caption = "Note: No data available for 2013-14 and 2019-21 (COVID-19 pandemic)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# VISUALIZATION 2: Line Chart with Points
# Primary Author: Ritvik Kothapalyam
# Reviewer: Amal Parekh
# ==============================================================================

plot2 <- ggplot(pssa_data, aes(x = year, y = percentage, 
                               color = score_level, group = score_level)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Advanced" = "#2E7D32",
      "Proficient" = "#66BB6A",
      "Basic" = "#FFA726",
      "Below Basic" = "#E53935"
    ),
    name = "Proficiency Level"
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    breaks = seq(0, 40, 5)
  ) +
  labs(
    title = "Trends in PSSA Science Proficiency Levels",
    subtitle = "Individual proficiency level trajectories (2008-2024)",
    x = "School Year",
    y = "Percentage of Students",
    caption = "Data source: Pennsylvania State System of Assessment"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# VISUALIZATION 3: Combined Proficiency Rate
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam
# ==============================================================================

plot3 <- ggplot(proficiency_summary, aes(x = year, y = combined_proficient)) +
  geom_line(color = "#1976D2", size = 1.5) +
  geom_point(color = "#1976D2", size = 4) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(55, 75)
  ) +
  labs(
    title = "Combined Proficiency Rate (Advanced + Proficient)",
    subtitle = "Tracking overall student proficiency in science over time",
    x = "School Year",
    y = "Percentage of Proficient/Advanced Students",
    caption = "Red line indicates 60% benchmark"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# VISUALIZATION 4: Grouped Bar Chart by Year
# Primary Author: Ritvik Kothapalyam
# Reviewer: Amal Parekh
# ==============================================================================

plot4 <- ggplot(pssa_data, aes(x = factor(year), y = percentage, 
                               fill = score_level)) +
  geom_col(position = "dodge", width = 0.8) +
  scale_fill_manual(
    values = c(
      "Advanced" = "#2E7D32",
      "Proficient" = "#66BB6A",
      "Basic" = "#FFA726",
      "Below Basic" = "#E53935"
    ),
    name = "Proficiency Level"
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, 40)
  ) +
  labs(
    title = "Year-by-Year Comparison of PSSA Science Proficiency Levels",
    subtitle = "Grouped bar chart showing distribution across all years",
    x = "School Year",
    y = "Percentage of Students"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# VISUALIZATION 5: Faceted Distribution Plots
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam
# ==============================================================================

plot5 <- ggplot(pssa_data, aes(x = year, y = percentage, fill = score_level)) +
  geom_area(alpha = 0.7) +
  facet_wrap(~score_level, ncol = 2) +
  scale_fill_manual(
    values = c(
      "Advanced" = "#2E7D32",
      "Proficient" = "#66BB6A",
      "Basic" = "#FFA726",
      "Below Basic" = "#E53935"
    )
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "Individual Proficiency Level Trends (Faceted View)",
    subtitle = "Separate panels for each proficiency category",
    x = "School Year",
    y = "Percentage of Students"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# VISUALIZATION 6: Violin/Box Plot Distribution
# Primary Author: Ritvik Kothapalyam
# Reviewer: Amal Parekh
# ==============================================================================

plot6 <- ggplot(pssa_data, aes(x = score_level, y = percentage, 
                               fill = score_level)) +
  geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
  scale_fill_manual(
    values = c(
      "Advanced" = "#2E7D32",
      "Proficient" = "#66BB6A",
      "Basic" = "#FFA726",
      "Below Basic" = "#E53935"
    )
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "Distribution of Proficiency Percentages Across All Years",
    subtitle = "Violin plots showing data spread and central tendency",
    x = "Proficiency Level",
    y = "Percentage of Students"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# VISUALIZATION 7: Heatmap
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam
# ==============================================================================

plot7 <- ggplot(pssa_data, aes(x = factor(year), y = score_level, 
                               fill = percentage)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_gradient2(
    low = "#E53935",
    mid = "#FFA726",
    high = "#2E7D32",
    midpoint = 25,
    name = "Percentage",
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "PSSA Science Proficiency Heatmap",
    subtitle = "Color intensity represents percentage of students at each level",
    x = "School Year",
    y = "Proficiency Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# ==============================================================================
# VISUALIZATION 8: Stacked Bar Chart (100%)
# Primary Author: Ritvik Kothapalyam
# Reviewer: Amal Parekh
# ==============================================================================

plot8 <- ggplot(pssa_data, aes(x = factor(year), y = percentage, 
                               fill = score_level)) +
  geom_col(position = "fill", width = 0.9) +
  scale_fill_manual(
    values = c(
      "Advanced" = "#2E7D32",
      "Proficient" = "#66BB6A",
      "Basic" = "#FFA726",
      "Below Basic" = "#E53935"
    ),
    name = "Proficiency Level"
  ) +
  scale_y_continuous(
    labels = label_percent(),
    breaks = seq(0, 1, 0.1)
  ) +
  labs(
    title = "Proportional Distribution of PSSA Science Proficiency",
    subtitle = "100% stacked bar chart showing relative composition",
    x = "School Year",
    y = "Proportion of Students"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# VISUALIZATION 9: Density Ridge Plot
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam
# ==============================================================================

# Prepare data for ridge plot
pssa_expanded <- pssa_data %>%
  group_by(time_frame, score_level) %>%
  slice(rep(1, round(percentage))) %>%
  ungroup()

plot9 <- ggplot(pssa_data, aes(x = percentage, y = score_level, 
                               fill = score_level)) +
  geom_density_ridges(alpha = 0.7, scale = 0.9) +
  scale_fill_manual(
    values = c(
      "Advanced" = "#2E7D32",
      "Proficient" = "#66BB6A",
      "Basic" = "#FFA726",
      "Below Basic" = "#E53935"
    )
  ) +
  scale_x_continuous(
    labels = label_percent(scale = 1)
  ) +
  labs(
    title = "Density Distribution of Proficiency Percentages",
    subtitle = "Ridge plot showing overlap and spread of each category",
    x = "Percentage of Students",
    y = "Proficiency Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

# ==============================================================================
# VISUALIZATION 10: Change from Baseline
# Primary Author: Ritvik Kothapalyam
# Reviewer: Amal Parekh
# ==============================================================================

# Calculate change from baseline (2008-09)
baseline_data <- pssa_data %>%
  filter(year == 2008) %>%
  select(score_level, baseline = percentage)

change_data <- pssa_data %>%
  left_join(baseline_data, by = "score_level") %>%
  mutate(change = percentage - baseline)

plot10 <- ggplot(change_data, aes(x = year, y = change, 
                                  color = score_level, group = score_level)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Advanced" = "#2E7D32",
      "Proficient" = "#66BB6A",
      "Basic" = "#FFA726",
      "Below Basic" = "#E53935"
    ),
    name = "Proficiency Level"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%")
  ) +
  labs(
    title = "Change in Proficiency from 2008-09 Baseline",
    subtitle = "Tracking gains and losses relative to initial year",
    x = "School Year",
    y = "Percentage Point Change",
    caption = "Positive values indicate improvement; negative values indicate decline"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# DISPLAY ALL PLOTS IN RSTUDIO
# Primary Author: Amal Parekh
# Reviewer: Ritvik Kothapalyam
# ==============================================================================

# Store all plots in a list
plot_list <- list(
  "1. Stacked Area Chart" = plot1,
  "2. Line Chart with Points" = plot2,
  "3. Combined Proficiency Rate" = plot3,
  "4. Grouped Bar Chart" = plot4,
  "5. Faceted Distribution Plots" = plot5,
  "6. Violin/Box Plot Distribution" = plot6,
  "7. Heatmap" = plot7,
  "8. 100% Stacked Bar Chart" = plot8,
  "9. Density Ridge Plot" = plot9,
  "10. Change from Baseline" = plot10
)

# Display each plot with explicit rendering
cat("\n=== Displaying all plots in RStudio Plots pane ===\n")
cat("Use the arrow buttons (◄ ►) in the Plots pane to navigate between plots\n\n")

for (i in seq_along(plot_list)) {
  cat(paste0("Rendering plot ", i, ": ", names(plot_list)[i], "\n"))
  print(plot_list[[i]])
  Sys.sleep(0.5)  # Small delay to ensure plot renders
}

cat("\n=== All 10 plots have been rendered! ===\n")
cat("Navigate through them using the arrows in the Plots pane.\n")

# Save plots to Downloads folder
save_path <- "/Users/amalparekh/Downloads"

ggsave(
  filename = file.path(save_path, "pssa_01_stacked_area.png"),
  plot = plot1,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(save_path, "pssa_02_line_chart.png"),
  plot = plot2,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(save_path, "pssa_03_combined_proficiency.png"),
  plot = plot3,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(save_path, "pssa_04_grouped_bar.png"),
  plot = plot4,
  width = 12,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(save_path, "pssa_05_faceted_trends.png"),
  plot = plot5,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(save_path, "pssa_06_violin_distribution.png"),
  plot = plot6,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(save_path, "pssa_07_heatmap.png"),
  plot = plot7,
  width = 12,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(save_path, "pssa_08_stacked_bar_100.png"),
  plot = plot8,
  width = 12,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(save_path, "pssa_09_density_ridge.png"),
  plot = plot9,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(save_path, "pssa_10_change_baseline.png"),
  plot = plot10,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\n=== All plots saved successfully to Downloads folder ===\n")
cat(paste0("Location: ", save_path, "\n"))
cat("Total plots created: 10\n")

# ==============================================================================
# KEY INSIGHTS
# ==============================================================================

cat("\n=== KEY FINDINGS ===\n")
cat("1. Assessment Change (2012-13): Significant jump in Advanced/Proficient\n")
cat("2. Pre-Pandemic Peak (2018-19): Highest combined proficiency at 68%\n")
cat("3. Post-Pandemic Impact (2021-22): Drop to 62.2% proficiency\n")
cat("4. Recent Recovery (2023-24): Improving to 65.8% proficiency\n")
cat("5. Below Basic Trend: Generally declining over time (good sign)\n")
