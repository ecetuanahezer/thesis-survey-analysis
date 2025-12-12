library(ggplot2)
library(dplyr)
library(forcats)
interaction_data <- data_long %>%
  group_by(Attribution_IV, AI_Use_IV) %>%
  summarise(
    # Calculate the Mean Quality for each 2x2 group
    Mean_Quality = mean(Creativity, na.rm = TRUE),
    # Calculate the Standard Error (SE) for error bars
    SE_Quality = sd(Creativity, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# --- STEP 2: Visualize the Interaction using ggplot2 ---

# 2a. Optional: Ensure the X-axis is ordered correctly (Human then AI)
interaction_data$AI_Use_IV <- fct_relevel(interaction_data$AI_Use_IV, "Human", "AI_Assisted")

plot_quality_interaction <- ggplot(interaction_data, 
                                   aes(x = AI_Use_IV, 
                                       y = Mean_Quality, 
                                       group = Attribution_IV, 
                                       color = Attribution_IV)) +
  
  # Add lines to show the interaction pattern
  geom_line(size = 1.2) +
  
  # Add points for the mean of each group
  geom_point(size = 4, shape = 19) +
  
  # Add error bars based on the calculated Standard Error (SE)
  geom_errorbar(aes(ymin = Mean_Quality - SE_Quality, 
                    ymax = Mean_Quality + SE_Quality), 
                width = 0.15, 
                size = 0.8) +
  
  # Set professional labels and title
  labs(
    title = "The Interaction of Attribution x AI Use on Perceived Creativity",
    x = "AI Use",
    y = "Perceived Creativity (Mean)",
    color = "Attribution Condition" # Relabel the legend title
  ) +
  
  # Use a clean, journal-appropriate theme
  theme_classic() +
  
  # Customize appearance (e.g., center title, move legend)
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    axis.title = element_text(size = 12, face = "bold")
  ) +
  
  # Set Y-axis limits to match the full range of your Likert scale (e.g., 1 to 7)
  # Adjust this based on your actual scale min/max if needed.
  ylim(1, 5)

# Display the plot
print(plot_quality_interaction)

interaction_data <- data_long %>%
  group_by(Attribution_IV, AI_Use_IV) %>%
  summarise(
    # Calculate the Mean Quality for each 2x2 group
    Mean_Quality = mean(Quality, na.rm = TRUE),
    # Calculate the Standard Error (SE) for error bars
    SE_Quality = sd(Quality, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# --- STEP 2: Visualize the Interaction using ggplot2 ---

# 2a. Optional: Ensure the X-axis is ordered correctly (Human then AI)
interaction_data$AI_Use_IV <- fct_relevel(interaction_data$AI_Use_IV, "Human", "AI_Assisted")

plot_quality_interaction <- ggplot(interaction_data, 
                                   aes(x = AI_Use_IV, 
                                       y = Mean_Quality, 
                                       group = Attribution_IV, 
                                       color = Attribution_IV)) +
  
  # Add lines to show the interaction pattern
  geom_line(size = 1.2) +
  
  # Add points for the mean of each group
  geom_point(size = 4, shape = 19) +
  
  # Add error bars based on the calculated Standard Error (SE)
  geom_errorbar(aes(ymin = Mean_Quality - SE_Quality, 
                    ymax = Mean_Quality + SE_Quality), 
                width = 0.15, 
                size = 0.8) +
  
  # Set professional labels and title
  labs(
    title = "The Interaction of Attribution x AI Use on Perceived Quality",
    x = "AI Use",
    y = "Perceived Quality (Mean)",
    color = "Attribution Condition" # Relabel the legend title
  ) +
  
  # Use a clean, journal-appropriate theme
  theme_classic() +
  
  # Customize appearance (e.g., center title, move legend)
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    axis.title = element_text(size = 12, face = "bold")
  ) +
  
  # Set Y-axis limits to match the full range of your Likert scale (e.g., 1 to 7)
  # Adjust this based on your actual scale min/max if needed.
  ylim(1, 5)

# Display the plot
print(plot_quality_interaction)

interaction_data <- data_long %>%
  group_by(Attribution_IV, AI_Use_IV) %>%
  summarise(
    # Calculate the Mean Quality for each 2x2 group
    Mean_Quality = mean(Willingness, na.rm = TRUE),
    # Calculate the Standard Error (SE) for error bars
    SE_Quality = sd(Willingness, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

# --- STEP 2: Visualize the Interaction using ggplot2 ---

# 2a. Optional: Ensure the X-axis is ordered correctly (Human then AI)
interaction_data$AI_Use_IV <- fct_relevel(interaction_data$AI_Use_IV, "Human", "AI_Assisted")

plot_quality_interaction <- ggplot(interaction_data, 
                                   aes(x = AI_Use_IV, 
                                       y = Mean_Quality, 
                                       group = Attribution_IV, 
                                       color = Attribution_IV)) +
  
  # Add lines to show the interaction pattern
  geom_line(size = 1.2) +
  
  # Add points for the mean of each group
  geom_point(size = 4, shape = 19) +
  
  # Add error bars based on the calculated Standard Error (SE)
  geom_errorbar(aes(ymin = Mean_Quality - SE_Quality, 
                    ymax = Mean_Quality + SE_Quality), 
                width = 0.15, 
                size = 0.8) +
  
  # Set professional labels and title
  labs(
    title = "Attribution x AI Use on Perceived Willingness",
    x = "AI Use",
    y = "Perceived Willingness (Mean)",
    color = "Attribution Condition" # Relabel the legend title
  ) +
  
  # Use a clean, journal-appropriate theme
  theme_classic() +
  
  # Customize appearance (e.g., center title, move legend)
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    axis.title = element_text(size = 12, face = "bold")
  ) +
  
  # Set Y-axis limits to match the full range of your Likert scale (e.g., 1 to 7)
  # Adjust this based on your actual scale min/max if needed.
  ylim(1, 5)

# Display the plot
print(plot_quality_interaction)

