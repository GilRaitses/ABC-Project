# Install and load necessary packages
# install.packages("dplyr")
# install.packages("fuzzyjoin")
# install.packages("tagtools")
# install.packages("tidyr")
# install.packages("purrr")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("patchwork")

library(dplyr)
library(purrr)
library(fuzzyjoin)
library(tagtools)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)

# Load CSV files into R dataframes
lavCalf <- read.csv("data/mn09_203a.csv")
logLavCalf <- read.csv("data/log_mn09_203a.csv")

# Convert to data frames
df <- as.data.frame(lavCalf)
logDf <- as.data.frame(logLavCalf)

# Function to convert strings to camel case
to_camel_case <- function(string, is_state = FALSE) {
  words <- strsplit(string, " |_")[[1]]
  if (is_state) {
    if (length(words) > 1) {
      camel_case <- paste0(tolower(words[1]), 
                           paste0(toupper(substr(words[-1], 1, 1)), tolower(substr(words[-1], 2, nchar(words[-1])))))
    } else {
      camel_case <- tolower(words[1])
    }
  } else {
    camel_case <- paste0(toupper(substr(words, 1, 1)), tolower(substr(words, 2, nchar(words))), collapse = "")
  }
  return(camel_case)
}

# Function to generate unique group names
generate_unique_group_name <- function(state, event, existing_names) {
  state <- as.character(state)
  event <- as.character(event)
  if (tolower(state) == tolower(event)) {
    base_name <- to_camel_case(state, TRUE)
  } else {
    base_name <- paste0(to_camel_case(state, TRUE), to_camel_case(event, FALSE))
  }
  name <- base_name
  i <- 1
  while (name %in% existing_names) {
    name <- paste0(base_name, i)
    i <- i + 1
  }
  return(name)
}

# Initialize the merged data frame and existing names list
merged_df <- data.frame()
existing_names <- character()

# Initialize counters for unlabeled segments and labeled segments
unlabeled_counter <- 1
labeled_segment_counter <- 1

for (i in 1:nrow(logDf)) {
  if (i == 1) {
    # Handle frames before the first event
    start <- -Inf
    end <- logDf[i, "eventStart"] - 1
  } else {
    # Handle frames between events
    start <- logDf[i - 1, "eventEnd"] + 1
    end <- logDf[i, "eventStart"] - 1
  }
  
  if (start <= end) {
    # Unlabeled frames
    unlabeled_sensor_data <- df %>%
      filter(s >= start & s <= end) %>%
      mutate(
        state = NA,
        event = NA,
        group_name = paste0("unlabeled", unlabeled_counter)
      )
    unlabeled_counter <- unlabeled_counter + 1
    merged_df <- bind_rows(merged_df, unlabeled_sensor_data)
  }
  
  if (i <= nrow(logDf) - 1) {
    # Labeled frames
    event_data <- logDf[i, ]
    start <- event_data$eventStart
    end <- event_data$eventEnd
    state <- event_data$state
    event <- event_data$event
    
    event_sensor_data <- df %>%
      filter(s >= start & s <= end) %>%
      rowwise() %>%
      mutate(
        state = to_camel_case(state),
        event = to_camel_case(event),
        group_name = generate_unique_group_name(state, event, existing_names),
        segmentid = paste0("labeledSegment", labeled_segment_counter)
      ) %>%
      ungroup()
    labeled_segment_counter <- labeled_segment_counter + 1
    existing_names <- c(existing_names, unique(event_sensor_data$group_name))
    merged_df <- bind_rows(merged_df, event_sensor_data)
  }
}

# Perform the left join to merge labeled and unlabeled data
final_df <- df %>% left_join(merged_df, by = "s", suffix = c("", ".y"))

# Exclude unwanted columns
final_df <- final_df %>% select(-ends_with(".y"))

# Split the final data frame into labeled and unlabeled tables
labeled_df <- final_df %>% filter(!is.na(state) & !is.na(event))
unlabeled_df <- final_df %>% filter(is.na(state) & is.na(event))

# View the labeled and unlabeled data frames
print("Labeled Data:")
print(labeled_df)
write.csv(labeled_df, "output/labeled_df.csv", row.names = FALSE)

# Function to calculate norm jerk for each event
calculate_event_normjerk <- function(df) {
  accelerometer_data <- as.matrix(df %>% select(Aw.1, Aw.2, Aw.3))
  sample_rate <- df$fs[1] # Assuming fs is constant within an event
  
  # Calculate the norm of the acceleration (norm2 of acceleration data)
  acc_norm <- sqrt(rowSums(accelerometer_data^2))
  
  # Calculate the differential of the acceleration norm
  jerk <- c(0, diff(acc_norm) * sample_rate) # multiply by sample rate to convert to jerk
  
  return(jerk)
}

# Function to calculate norm jerk for a chunk of data and keep required variables
calculate_normjerk_for_chunk <- function(chunk) {
  chunk %>%
    group_by(group_name) %>%
    mutate(normjerk = calculate_event_normjerk(cur_data())) %>%
    ungroup()
}

# Split the labeled data into chunks
chunk_size <- 10000  # Adjust this for memory limits
labeled_chunks <- split(labeled_df, ceiling(seq_along(labeled_df$group_name) / chunk_size))

# Apply the norm jerk calculation to each chunk
normjerk_labeled_chunks <- lapply(labeled_chunks, function(chunk) {
  result <- tryCatch({
    calculate_normjerk_for_chunk(chunk)
  }, error = function(e) {
    message("Error in chunk: ", conditionMessage(e))
    return(NULL)
  })
  result
})
normjerk_labeled_chunks <- normjerk_labeled_chunks[!sapply(normjerk_labeled_chunks, is.null)]

# Combine the chunks
labeled_normjerk_df <- bind_rows(normjerk_labeled_chunks)

# Ensure the normjerk column exists
if(!"normjerk" %in% colnames(labeled_normjerk_df)) stop("normjerk column not found in labeled_normjerk_df")

# Check the first few rows of labeled_normjerk_df to ensure normjerk is calculated
print("First few rows of labeled_normjerk_df with normjerk:")
print(head(labeled_normjerk_df))

eventData_df <- labeled_normjerk_df

# Define thresholds for deep dives and deep feeding
deep_dive_threshold <- 10  # Depth in meters for a dive to be considered "deep"
deep_feeding_threshold <- 10  # Depth in meters for deep feeding

# Function to label dive phases based on depth and time thresholds
label_dive_phases <- function(df) {
  df <- df %>%
    arrange(s) %>%  # Ensure the data is sorted by frame number
    mutate(
      phase = case_when(
        p >= deep_dive_threshold ~ "Deep Phase",
        p < deep_dive_threshold & p >= 5 ~ "Shallow Phase",
        p >= 0 ~ "Surface Phase"
      )
    )
  
  return(df)
}

# Apply the function to label dive phases
eventData_df <- label_dive_phases(eventData_df)

# View the updated dataframe with dive phases
head(eventData_df)

write.csv(eventData_df, "output/eventData_df.csv", row.names = FALSE)

# Function to convert radians to degrees
convert_to_degrees <- function(radians) {
  return(radians * 180 / pi)
}

# Compute statistics for deep dives
deep_dive_stats <- eventData_df %>%
  filter(p >= deep_dive_threshold) %>%
  summarise(
    mean_depth = mean(p),
    max_depth = max(p),
    min_depth = min(p),
    mean_duration = mean(s),
    total_duration = sum(s)
  )

# Calculate statistics for deep feeding
deep_feeding_stats <- eventData_df %>%
  filter(p >= deep_feeding_threshold) %>%
  summarise(
    mean_depth = mean(p, na.rm = TRUE),
    max_depth = max(p, na.rm = TRUE),
    min_depth = min(p, na.rm = TRUE),
    mean_duration = mean(s, na.rm = TRUE),
    total_duration = sum(s, na.rm = TRUE)
  )

# View the calculated statistics
print(deep_feeding_stats)

# Define thresholds for surface and shallow phases
surface_threshold <- 0  # Depth in meters for surface feeding
shallow_dive_threshold <- 10  # Depth in meters for shallow dives
shallow_feeding_threshold <- 10  # Depth in meters for shallow feeding

# Compute statistics for surface feeding
surface_feeding_stats <- eventData_df %>%
  filter(p <= surface_threshold) %>%
  summarise(
    mean_depth = mean(p),
    max_depth = max(p),
    min_depth = min(p),
    mean_duration = mean(s),
    total_duration = sum(s)
  )

# Compute statistics for shallow dives and shallow feeding
shallow_dive_stats <- eventData_df %>%
  filter(p > surface_threshold & p <= shallow_dive_threshold) %>%
  summarise(
    mean_depth = mean(p),
    max_depth = max(p),
    min_depth = min(p),
    mean_duration = mean(s),
    total_duration = sum(s)
  )

shallow_feeding_stats <- eventData_df %>%
  filter(p > shallow_dive_threshold & p <= shallow_feeding_threshold) %>%
  summarise(
    mean_depth = mean(p),
    max_depth = max(p),
    min_depth = min(p),
    mean_duration = mean(s),
    total_duration = sum(s)
  )

# Count instances of each phase
phase_counts <- eventData_df %>%
  group_by(phase) %>%
  summarise(count = n())

# Function to convert degrees for head to continuous
convert_degrees_continuous <- function(degrees) {
  delta_degrees <- diff(degrees)
  jumps <- which(abs(delta_degrees) > 180)
  continuous_degrees <- degrees
  
  for (jump in jumps) {
    if (delta_degrees[jump] > 180) {
      continuous_degrees[(jump + 1):length(degrees)] <- continuous_degrees[(jump + 1):length(degrees)] - 360
    } else if (delta_degrees[jump] < -180) {
      continuous_degrees[(jump + 1):length(degrees)] <- continuous_degrees[(jump + 1):length(degrees)] + 360
    }
  }
  
  return(continuous_degrees)
}

# Function to create composite time series dashboards for each event segment
create_composite_time_series_dashboards <- function(df, original_df, output_dir, deep_dive_threshold = 10, shallow_dive_threshold = 5, surface_threshold = 0) {
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  unique_base_groups <- unique(df$event)
  
  for (base_group in unique_base_groups) {
    base_group_dir <- file.path(output_dir, base_group)
    
    # Ensure the base group directory exists
    if (!dir.exists(base_group_dir)) {
      dir.create(base_group_dir, recursive = TRUE)
    }
    
    base_group_events <- df %>% filter(event == base_group)
    
    for (event_id in unique(base_group_events$group_name)) {
      eventData_df <- base_group_events %>% filter(group_name == event_id)
      
      # Ensure eventData_df is not empty
      if (nrow(eventData_df) == 0) {
        print(paste("Skipping event ID:", event_id, "due to no data"))
        next
      }
      
      # Convert frames to seconds
      sample_rate <- eventData_df$fs[1]  # Assuming fs is constant within an event
      eventData_df <- eventData_df %>% mutate(time_sec = s / sample_rate)
      
      # Ensure time_sec is not empty or contains only missing values
      if (all(is.na(eventData_df$time_sec)) || length(eventData_df$time_sec) == 0) {
        print(paste("Skipping event ID:", event_id, "due to invalid time_sec"))
        next
      }
      
      # Convert pitch, roll, head from radians to degrees and fix head degrees to be continuous
      eventData_df <- eventData_df %>%
        mutate(
          pitch_deg = pitch * 180 / pi,
          roll_deg = roll * 180 / pi,
          head_deg = convert_degrees_continuous(head * 180 / pi)
        )
      
      # Calculate start and end time in minutes and seconds
      start_time_seconds <- min(eventData_df$time_sec, na.rm = TRUE)
      end_time_seconds <- max(eventData_df$time_sec, na.rm = TRUE)
      start_depth <- -eventData_df$p[1]
      
      # Ensure start_time_seconds and end_time_seconds are valid
      if (is.infinite(start_time_seconds) || is.infinite(end_time_seconds)) {
        print(paste("Skipping event ID:", event_id, "due to invalid start or end time"))
        next
      }
      
      start_time <- paste0(floor(start_time_seconds / 60), "m ", round(start_time_seconds %% 60, 1), "s")
      end_time <- paste0(floor(end_time_seconds / 60), "m ", round(end_time_seconds %% 60, 1), "s")
      duration_seconds <- end_time_seconds - start_time_seconds
      duration <- paste0(floor(duration_seconds / 60), "m ", round(duration_seconds %% 60, 1), "s")
      
      # Create pitch, roll, head plot
      prh_plot <- ggplot(eventData_df) +
        geom_line(aes(x = time_sec, y = pitch_deg, color = "Pitch")) +
        geom_line(aes(x = time_sec, y = roll_deg, color = "Roll")) +
        geom_line(aes(x = time_sec, y = head_deg, color = "Head")) +
        labs(title = paste("Pitch, Roll, Head for", event_id, "from", start_time, "to", end_time, "(Duration:", duration, ")"),
             x = "Time (s)", y = "Magnitude of Change (Degrees)") +
        theme_minimal() +
        theme(legend.position = "top") +
        scale_x_continuous(breaks = seq(0, max(eventData_df$time_sec, na.rm = TRUE), by = 10))
      
      # Create normjerk plot
      normjerk_plot <- ggplot(eventData_df) +
        geom_line(aes(x = time_sec, y = normjerk, color = "Norm Jerk")) +
        labs(title = paste("Norm Jerk for", event_id, "from", start_time, "to", end_time, "(Duration:", duration, ")"),
             x = "Time (s)", y = "Norm Jerk (m/s^3)") +
        theme_minimal() +
        theme(legend.position = "top") +
        scale_x_continuous(breaks = seq(0, max(eventData_df$time_sec, na.rm = TRUE), by = 10))
      
      # Create depth and temperature plot
      p_tempr_plot <- ggplot(eventData_df) +
        geom_line(aes(x = time_sec, y = -p, color = "Depth")) +
        geom_line(aes(x = time_sec, y = tempr, color = "Temperature")) +
        labs(title = paste("Depth and Temperature for", event_id, "from", start_time, "to", end_time, "(Duration:", duration, ")"),
             x = "Time (s)", y = "Depth (m) / Temperature (°C)") +
        scale_y_continuous(sec.axis = sec_axis(~ ., name = "Temperature (°C)")) +
        theme_minimal() +
        theme(legend.position = "top") +
        scale_x_continuous(breaks = seq(0, max(eventData_df$time_sec, na.rm = TRUE), by = 10))
      
      # Calculate frame padding
      start_frame <- min(eventData_df$s)
      end_frame <- max(eventData_df$s)
      padding_frames <- 5 * 60 * sample_rate  # 5 minutes of padding
      padded_start_frame <- max(start_frame - padding_frames, 0)
      padded_end_frame <- min(end_frame + padding_frames, max(original_df$s))
      
      # Extract depth data for the 10-minute window
      depth_data <- original_df %>%
        filter(s >= padded_start_frame & s <= padded_end_frame) %>%
        mutate(time_sec = s / sample_rate)
      
      # Create a temporary table for phase points
      phase_points <- eventData_df %>%
        group_by(phase) %>%
        slice(1) %>%
        ungroup() %>%
        mutate(time_sec = s / sample_rate, depth = -p)
      
      # Create the 10-minute depth window plot with vertical lines marking segment start and end, and phase points
      depth_window_plot <- ggplot(depth_data) +
        geom_line(aes(x = time_sec, y = -p, color = "Depth")) +
        geom_vline(xintercept = start_frame / sample_rate, linetype = "dashed", color = "red", size = 0.5) +
        geom_vline(xintercept = end_frame / sample_rate, linetype = "dashed", color = "red", size = 0.5) +
        geom_point(data = phase_points, aes(x = time_sec, y = depth), color = "black", size = 2) +
        geom_text(data = phase_points, aes(x = time_sec, y = depth, label = phase), vjust = -0.5, hjust = 1.2, color = "black", size = 3, fontface = "italic", alpha = 0.7) +
        geom_point(aes(x = start_time_seconds, y = start_depth), color = "forestgreen", size = 4) +
        geom_point(aes(x = end_time_seconds, y = -eventData_df$p[nrow(eventData_df)]), color = "red3", size = 4) +
        geom_hline(yintercept = -deep_dive_threshold, linetype = "dashed", color = "blue", size = 0.5) +
        geom_hline(yintercept = -shallow_dive_threshold, linetype = "dashed", color = "green", size = 0.5) +
        geom_hline(yintercept = -surface_threshold, linetype = "dashed", color = "purple", size = 0.5) +
        annotate("text", x = mean(c(start_time_seconds, end_time_seconds)), y = start_depth, label = event_id, vjust = -3, hjust = 0.5, color = "blue", size = 3) +
        labs(title = "Phase Level View", x = "Time (s)", y = "Depth (m)") +
        theme_minimal() +
        theme(legend.position = "top") +
        scale_x_continuous(breaks = seq(min(depth_data$time_sec), max(depth_data$time_sec), by = 60))
      
      # Combine plots into one
      combined_plot <- (prh_plot / normjerk_plot / p_tempr_plot / depth_window_plot) + plot_layout(ncol = 1)
      
      # Save the combined plot
      plot_name <- file.path(base_group_dir, paste(event_id, "composite_dashboard.png", sep = "_"))
      print(paste("Saving plot:", plot_name)) # Debugging statement
      ggsave(plot_name, plot = combined_plot, width = 10, height = 20)
    }
  }
}

# Define the output directory
output_dir <- "output/event_composite_dashboards"

# Create and save composite time series dashboards
create_composite_time_series_dashboards(eventData_df, lavCalf, output_dir)
