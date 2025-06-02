# chart01.R

# function to create the benchmark chart
create_benchmark_chart <- function() {
  # Load required libraries 
  library(ggplot2)
  library(dplyr)
  library(scales)
  
  # Create the comparison data 
  benchmark_data <- tibble(
    category = c("Industry Standard", "Our Current Spend"),
    spend_millions = c(2.8, 50),
    color_code = c("benchmark", "current")
  )
  
  # Generate the ggplot2 visualization 
  chart1 <- ggplot(benchmark_data, aes(x = category, y = spend_millions, fill = color_code)) +
    geom_col(width = 0.6, alpha = 0.9) +
    
    # Geoms
    geom_text(aes(label = paste0("$", spend_millions, "M")),
              vjust = -0.5,
              size = 6,
              fontface = "bold",
              color = "black"
    ) +
    
    # Scales
    scale_fill_manual(values = c("benchmark" = "#95a5a6", "current" = "#E74C3C")) +
    scale_y_continuous(
      labels = dollar_format(suffix = "M"),
      limits = c(0, 55),
      expand = c(0, 0)
    ) +
    
    # Labs
    labs(
      title = "We Spend 17x More Than Industry Benchmark",
      subtitle = "Annual spend comparison shows massive cost inefficiency",
      x = NULL,
      y = "Annual Spend",
      caption = "Source: Strategic Sourcing Analysis, XYZ Products"
    ) +
    
    # Theme
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "#2c3e50"),
      plot.subtitle = element_text(size = 12, color = "#7f8c8d"),
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 12, face = "bold"),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      plot.caption = element_text(size = 9, color = "#95a5a6")
    )
  
  return(chart1)
}

