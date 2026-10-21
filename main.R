# ==================================================
# 1. PACKAGES AND DATA
# ==================================================

# Run once if needed:
# install.packages(c("ggplot2", "readr", "dplyr", "stringr"))

library(ggplot2)
library(readr)
library(dplyr)
library(stringr)

wifi <- read_csv("final.csv")

building_colors <- c(
  "Carnigee"                = "#E6194B",
  "Pittsburgh"              = "#3CB44B",
  "Sage"                    = "#4363D8",
  "Amos_Eaton"              = "#F58231",
  "Folsom_Library"          = "#911EB4",
  "Muller"                  = "#42D4F4",
  "Lally"                   = "#F032E6",
  "Vorhees_Computer_Center" = "#808000",
  "DCC"                     = "#800000",
  "Union"                   = "#469990",
  "MILL_Low"                = "#9A6324",
  "Low"                     = "#000075",
  "JEC"                     = "#000000",
  "Academy_Hall"            = "#A9A900"
)

buildings <- names(building_colors)

wifi <- wifi %>%
  mutate(
    Observation = row_number(),
    Location = str_replace(Location, "^Pitsburg_", "Pittsburgh_"),
    Building = str_extract(
      Location,
      paste0("^(", paste(buildings, collapse = "|"), ")(?=_|$)")
    ),
    Building = factor(Building, levels = buildings),
    across(
      c(
        Download_Speed_mbps, Upload_speed_mbps,
        Loaded_Latency_ms, Unloaded_Latency_ms
      ),
      as.numeric
    ),
    Latency_Increase_ms = Loaded_Latency_ms - Unloaded_Latency_ms
  )

# Check for unmatched building names
print(
  wifi %>%
    filter(is.na(Building)) %>%
    distinct(Location)
)


# ==================================================
# 2. REUSABLE GRAPH FUNCTIONS
# ==================================================

building_labels <- function(x) {
  str_replace_all(x, "_", " ")
}

building_scale <- function() {
  scale_color_manual(
    values = building_colors,
    labels = building_labels,
    drop = FALSE
  )
}

building_axis <- function() {
  scale_x_discrete(labels = building_labels)
}

# Scatter plot colored by building
scatter_plot <- function(data, x, y, title, x_label, y_label) {
  ggplot(
    data,
    aes(x = .data[[x]], y = .data[[y]], color = Building)
  ) +
    geom_point(size = 3, alpha = 0.85) +
    building_scale() +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = "Building"
    ) +
    theme_minimal(base_size = 12)
}

# One average point per building
building_average_plot <- function(data, column, title, y_label) {
  scatter_plot(
    data, "Building", column, title, "Building", y_label
  ) +
    building_axis() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

# Box plot with individual readings
building_boxplot <- function(data, column, title, y_label) {
  ggplot(
    data,
    aes(x = Building, y = .data[[column]], color = Building)
  ) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(
      width = 0.15, height = 0,
      size = 2, alpha = 0.7
    ) +
    building_scale() +
    building_axis() +
    labs(title = title, x = "Building", y = y_label) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

# Side-by-side average bars
average_bar_plot <- function(data, title, y_label, legend_title,
                             colors) {
  ggplot(data, aes(Building, Average, fill = Type)) +
    geom_col(position = "dodge") +
    building_axis() +
    scale_fill_manual(values = colors) +
    labs(
      title = title,
      x = "Building",
      y = y_label,
      fill = legend_title
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# ==================================================
# 3. BUILDING SUMMARIES
# ==================================================

building_averages <- wifi %>%
  group_by(Building) %>%
  summarise(
    Readings = n(),
    Download = mean(Download_Speed_mbps, na.rm = TRUE),
    Upload = mean(Upload_speed_mbps, na.rm = TRUE),
    Loaded = mean(Loaded_Latency_ms, na.rm = TRUE),
    Unloaded = mean(Unloaded_Latency_ms, na.rm = TRUE),
    Increase = mean(Latency_Increase_ms, na.rm = TRUE),
    .groups = "drop"
  )

print(building_averages)


# ==================================================
# 4. SPEED: INDIVIDUAL READINGS
# ==================================================

reading_download <- scatter_plot(
  wifi,
  "Observation", "Download_Speed_mbps",
  "Download Speed: Individual Readings",
  "Observation Number", "Download Speed (Mbps)"
)

reading_upload <- scatter_plot(
  wifi,
  "Observation", "Upload_speed_mbps",
  "Upload Speed: Individual Readings",
  "Observation Number", "Upload Speed (Mbps)"
)

reading_comparison <- scatter_plot(
  wifi,
  "Download_Speed_mbps", "Upload_speed_mbps",
  "Download vs. Upload: Individual Readings",
  "Download Speed (Mbps)", "Upload Speed (Mbps)"
)

print(reading_download)
print(reading_upload)
print(reading_comparison)


# ==================================================
# 5. SPEED: BUILDING AVERAGES
# ==================================================

building_download <- building_average_plot(
  building_averages,
  "Download",
  "Average Download Speed by Building",
  "Average Download Speed (Mbps)"
)

building_upload <- building_average_plot(
  building_averages,
  "Upload",
  "Average Upload Speed by Building",
  "Average Upload Speed (Mbps)"
)

building_comparison <- scatter_plot(
  building_averages,
  "Download", "Upload",
  "Average Download vs. Upload by Building",
  "Average Download Speed (Mbps)",
  "Average Upload Speed (Mbps)"
)

print(building_download)
print(building_upload)
print(building_comparison)

# Side-by-side bars matching your example
speed_averages_long <- bind_rows(
  building_averages %>%
    transmute(Building, Type = "Download", Average = Download),
  building_averages %>%
    transmute(Building, Type = "Upload", Average = Upload)
)

speed_average_bars <- average_bar_plot(
  speed_averages_long,
  "Average Download and Upload Speeds by Building",
  "Average Speed (Mbps)",
  "Speed Type",
  c("Download" = "steelblue", "Upload" = "darkorange")
)

print(speed_average_bars)


# ==================================================
# 6. SPEED: BUILDING BOX PLOTS
# ==================================================

download_boxplot <- building_boxplot(
  wifi,
  "Download_Speed_mbps",
  "Download Speed Distribution by Building",
  "Download Speed (Mbps)"
)

upload_boxplot <- building_boxplot(
  wifi,
  "Upload_speed_mbps",
  "Upload Speed Distribution by Building",
  "Upload Speed (Mbps)"
)

print(download_boxplot)
print(upload_boxplot)


# ==================================================
# 7. LATENCY: INDIVIDUAL READINGS
# ==================================================

unloaded_plot <- scatter_plot(
  wifi,
  "Observation", "Unloaded_Latency_ms",
  "Unloaded Latency: Individual Readings",
  "Observation Number", "Unloaded Latency (ms)"
)

loaded_plot <- scatter_plot(
  wifi,
  "Observation", "Loaded_Latency_ms",
  "Loaded Latency: Individual Readings",
  "Observation Number", "Loaded Latency (ms)"
)
latency_comparison <- scatter_plot(
  wifi,
  "Unloaded_Latency_ms",
  "Loaded_Latency_ms",
  "Unloaded vs. Loaded Latency: Individual Readings",
  "Unloaded Latency (ms, log10 scale)",
  "Loaded Latency (ms, log10 scale)"
) +
  scale_x_log10() +
  scale_y_log10()
   

print(latency_comparison)


print(unloaded_plot)
print(loaded_plot)
print(latency_comparison)




# ==================================================
# 8. LATENCY: BUILDING AVERAGES
# ==================================================

latency_averages_long <- bind_rows(
  building_averages %>%
    transmute(Building, Type = "Loaded", Average = Loaded),
  building_averages %>%
    transmute(Building, Type = "Unloaded", Average = Unloaded)
)

latency_average_plot <- average_bar_plot(
  latency_averages_long,
  "Average Latency by Building",
  "Average Latency (ms)",
  "Network Condition",
  c("Loaded" = "darkorange", "Unloaded" = "steelblue")
)

latency_building_comparison <- scatter_plot(
  building_averages,
  "Unloaded", "Loaded",
  "Average Unloaded vs. Loaded Latency by Building",
  "Average Unloaded Latency (ms)",
  "Average Loaded Latency (ms)"
) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = "grey50"
  )

print(latency_average_plot)
print(latency_building_comparison)


# ==================================================
# 9. LATENCY: BUILDING BOX PLOTS
# ==================================================

latency_long <- bind_rows(
  wifi %>%
    transmute(
      Building, Type = "Loaded", Latency = Loaded_Latency_ms
    ),
  wifi %>%
    transmute(
      Building, Type = "Unloaded", Latency = Unloaded_Latency_ms
    )
)

latency_boxplot <- building_boxplot(
  latency_long,
  "Latency",
  "Latency Distributions by Building",
  "Latency (ms)"
) +
  facet_wrap(~ Type, ncol = 1, scales = "free_y")

latency_increase_plot <- building_boxplot(
  wifi,
  "Latency_Increase_ms",
  "Latency Increase Under Load by Building",
  "Latency Increase (ms)"
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed", color = "grey50"
  ) +
  labs(subtitle = "Loaded latency minus unloaded latency")

print(latency_boxplot)
print(latency_increase_plot)


# ==================================================
# 10. SPEED: INDIVIDUAL READINGS WITH ELLIPSES
# ==================================================

# Keep groups with enough readings and variation
ellipse_data <- function(data, x_column, y_column) {
  data %>%
    filter(
      !is.na(Building),
      is.finite(.data[[x_column]]),
      is.finite(.data[[y_column]])
    ) %>%
    group_by(Building) %>%
    filter({
      x <- .data[[x_column]]
      y <- .data[[y_column]]
      
      if (length(x) < 4) {
        FALSE
      } else if (sd(x) == 0 || sd(y) == 0) {
        FALSE
      } else {
        abs(cor(x, y)) < 0.999999
      }
    }) %>%
    ungroup()
}

ellipse_plot <- function(data, x, y, title, x_label, y_label) {
  ggplot(
    data,
    aes(x = .data[[x]], y = .data[[y]], color = Building)
  ) +
    stat_ellipse(
      data = ellipse_data(data, x, y),
      aes(fill = Building, group = Building),
      geom = "polygon",
      type = "norm",
      level = 0.95,
      alpha = 0.12,
      show.legend = FALSE
    ) +
    geom_point(size = 3, alpha = 0.85) +
    building_scale() +
    scale_fill_manual(
      values = building_colors,
      drop = FALSE,
      guide = "none"
    ) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = "Building"
    ) +
    theme_minimal(base_size = 12)
}

download_ellipses <- ellipse_plot(
  wifi,
  "Observation", "Download_Speed_mbps",
  "Download Readings with Building Ellipses",
  "Observation Number", "Download Speed (Mbps)"
)

upload_ellipses <- ellipse_plot(
  wifi,
  "Observation", "Upload_speed_mbps",
  "Upload Readings with Building Ellipses",
  "Observation Number", "Upload Speed (Mbps)"
)

comparison_ellipses <- ellipse_plot(
  wifi,
  "Download_Speed_mbps", "Upload_speed_mbps",
  "Download vs. Upload with Building Ellipses",
  "Download Speed (Mbps)", "Upload Speed (Mbps)"
)

print(download_ellipses)
print(upload_ellipses)
print(comparison_ellipses)

