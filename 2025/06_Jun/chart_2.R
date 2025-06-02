# chart02.R

# function to create the supplier performance chart
create_supplier_performance_chart <- function() {
  # Load required libraries inside the function
  library(dplyr)
  library(scales)
  library(here) 
  
  # Combine data
  supplier_performance <- supplier_averages |>
    left_join(
      market_share_clean |> select(supplier, industry_share, us_share),
      by = "supplier"
    ) |>
    left_join(
      supplier_totals |> select(supplier, total_millions),
      by = "supplier"
    ) |>
    mutate(
      # Calculate cost per performance point (lower is better)
      cost_per_performance = total_millions / avg_score,
      
      # Create recommendation categories based on actual efficiency
      recommendation = case_when(
        supplier == "C" ~ "Recommended",
        supplier == "A" ~ "Secondary Option",
        supplier == "D" ~ "Current (Reduce)",
        supplier == "B" ~ "Avoid"
      ),
      
      # Create size categories
      spend_category = case_when(
        total_millions > 1.0 ~ "High Spend",
        total_millions > 0.1 ~ "Medium Spend",
        TRUE ~ "Low Spend"
      )
    )
  
  # Generate the ggplot2 visualization 
  chart2 <- ggplot(supplier_performance, aes(x = avg_score, y = cost_per_performance)) +
    
    # Geoms
    geom_vline(xintercept = 3.85, linetype = "dashed", color = "#95a5a6", alpha = 0.7, linewidth = 0.3) +
    geom_hline(yintercept = 0.10, linetype = "dashed", color = "#95a5a6", alpha = 0.7, linewidth = 0.3) +
    geom_point(aes(fill = recommendation),
               shape = 21, alpha = 0.8, size = 6
    ) +
    geom_text(aes(label = paste("Supplier", supplier)),
              hjust = -0.2, vjust = -0.5, size = 4, fontface = "bold"
    ) +
    geom_text(
      aes(label = ifelse(total_millions < 1,
                         paste0("$", round(total_millions * 1000), "K"),
                         paste0("$", round(total_millions, 1), "M")
      )),
      hjust = -0.5, vjust = 1.2, size = 3.5, color = "gray30"
    ) +
    
    # Annotate
    annotate("text",
             x = 3.5, y = 0.28, label = "Lower Performance\nHigher Cost",
             size = 3, color = "#7f8c8d", alpha = 0.8, hjust = 0.5
    ) +
    annotate("text",
             x = 4.35, y = 0.28, label = "Higher Performance\nHigher Cost",
             size = 3, color = "#7f8c8d", alpha = 0.8, hjust = 0.5
    ) +
    annotate("text",
             x = 3.5, y = 0.03, label = "Lower Performance\nLower Cost",
             size = 3, color = "#7f8c8d", alpha = 0.8, hjust = 0.5
    ) +
    annotate("text",
             x = 4.35, y = 0.03, label = "Higher Performance\nLower Cost",
             size = 3, color = "#7f8c8d", alpha = 0.9, hjust = 0.5
    ) +
    
    # Scales
    scale_fill_manual(values = c(
      "Recommended" = "#27AE60",
      "Secondary Option" = "#3498DB",
      "Current (Reduce)" = "#F39C12",
      "Avoid" = "#E74C3C"
    )) +
    scale_x_continuous(
      limits = c(3.2, 4.8),
      breaks = seq(3.2, 4.8, 0.2)
    ) +
    scale_y_continuous(
      labels = dollar_format(accuracy = 0.01),
      limits = c(0.01, 0.35)
    ) +
    
    # Labs
    labs(
      title = "Supplier C Offers Best Balance: Good Performance at Low Cost",
      subtitle = "Quadrant analysis reveals optimal supplier mix - focus on bottom-right efficiency",
      x = "Average Performance Score (1-5 scale, higher is better)",
      y = "Cost per Performance Point (lower is better)",
      caption = "Source: Strategic Sourcing Analysis, XYZ Products"
    ) +
    
    # Theme
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"),
      plot.subtitle = element_text(size = 11, color = "#7f8c8d"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      panel.grid = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      plot.caption = element_text(size = 9, color = "#95a5a6")
    ) +
    guides(
      fill = guide_legend(
        title = "Strategic Action",
        title.position = "top",
        override.aes = list(size = 4)
      )
    )

  return(chart2)
}

