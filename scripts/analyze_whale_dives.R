library(dplyr)
library(ggplot2)
library(scales)
eventData_df <- read.csv("data/eventData_df.csv")
dir.create("plots/Analytics", recursive = TRUE, showWarnings = FALSE)
ocean_colors <- c("Deep Phase" = "#1f77b4", "Shallow Phase" = "#17becf", "Surface Phase" = "#aec7e8", "Surfacing" = "#ff7f0e")

# Cluster Analysis of Dive Cycles
set.seed(123)
kmeans_result <- kmeans(eventData_df %>% select(s, p) %>% na.omit() %>% scale(), centers = 3)
eventData_df <- eventData_df %>%
  mutate(cluster = as.factor(kmeans_result$cluster))
cluster_colors <- c("1" = "#1f77b4", "2" = "#aec7e8", "3" = "#ffbb78")
ggplot(eventData_df, aes(x = s, y = -p, color = cluster)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_text(
    data = eventData_df %>%
      group_by(cluster) %>%
      summarise(
        s = mean(s),
        p = mean(p)
      ),
    aes(label = paste("Cluster", cluster)),
    vjust = 3.3, hjust = .2, size = 5, color = "lightsalmon4"
  ) +
  labs(
    title = "Cluster Analysis of Dive Cycles",
    x = "Dive Duration (H:M:S)",
    y = "Dive Depth (m)",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = cluster_colors) +
  scale_y_continuous(breaks = seq(-40, 0, 10), labels = scales::comma) +
  scale_x_time(labels = scales::date_format("%H:%M:%S"))
ggsave("plots/Analytics/cluster_analysis_dive_cycles.png")

# Dive Duration vs Depth
ocean_colors <- c("Deep Phase" = "#1f77b4", "Shallow Phase" = "#17becf", "Surface Phase" = "#aec7e8", "NA" = "grey")
ggplot(eventData_df, aes(x = s, y = -p)) +
  geom_point(aes(color = phase), alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", color = "#ff7f0e", size = 1) + # Sunset-like regression line color
  labs(
    title = "Dive Duration vs Depth",
    x = "Dive Duration (s)",
    y = "Dive Depth (m)",
    color = "Dive Phase"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = ocean_colors) +
  scale_y_continuous(breaks = seq(-40, 0, 10), labels = scales::comma)
ggsave("plots/Analytics/dive_duration_vs_depth.png")


#  Temporal Distribution and Sequencing of Events
phase_colors <- c("Deep Phase" = "#1f77b4", "Shallow Phase" = "#17becf", "Surface Phase" = "#aec7e8", "Surfacing" = "#ff7f0e")
eventData_df <- eventData_df %>%
  mutate(phase = ifelse(is.na(phase), "Surfacing", phase))
ggplot(eventData_df, aes(x = time_sec, y = phase, color = phase)) +
  geom_point(alpha = 0.6, size = 1.5) +
  labs(
    title = "Temporal Distribution and Sequencing of Events Across Phases",
    x = "Time (H:M:S)",
    y = "Phase",
    color = "Phase"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = phase_colors) +
  scale_x_time(labels = scales::date_format("%H:%M:%S")) +
  scale_y_discrete(labels = c("Deep Phase", "Shallow Phase", "Surface Phase", "Surfacing"))
ggsave("plots/Analytics/temporal_distribution.png")
