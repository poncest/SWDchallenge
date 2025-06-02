# chart03.R

# function to create the strategic supplier scenarios chart
create_strategic_scenario_chart <- function() {
  # Load required libraries 
  library(ggplot2)
  library(dplyr)
  library(scales)
  
  # Historical actual data from slide (2022-2024)
  actual_totals <- c(2.22, 2.41, 2.62)
  
  # Build scenarios starting from 2024 actual baseline
  scenarios_clean <- data.frame(
    year = rep(2022:2028, 3),
    scenario = rep(c("Status Quo", "Dual Supplier", "Single Supplier"), each = 7)
  ) |>
    mutate(
      # Calculate spend projections based on different strategies
      spend_millions = case_when(
        # Actual historical data (2022-2024) - same for all scenarios
        year <= 2024 ~ rep(actual_totals, 3)[match(year, rep(2022:2024, 3))],
        
        # Status Quo: 15% annual growth from 2024 baseline (unsustainable escalation)
        scenario == "Status Quo" & year > 2024 ~ 2.62 * (1.15^(year - 2024)),
        
        # Dual Supplier: Efficiency gains - moderate growth from 2024 baseline
        scenario == "Dual Supplier" & year > 2024 ~ 2.62 * (1.06^(year - 2024)),
        
        # Single Supplier: Maximum efficiency - minimal growth from 2024 baseline
        scenario == "Single Supplier" & year > 2024 ~ 2.62 * (1.03^(year - 2024))
      ),
      
      # Create scenario categories
      scenario_type = case_when(
        scenario == "Status Quo" ~ "Current Path",
        scenario == "Dual Supplier" ~ "Recommended",
        scenario == "Single Supplier" ~ "Aggressive"
      ),
      
      # Create actual vs forecast indicator
      period_type = ifelse(year <= 2024, "Actual", "Forecast")
    )
  
  # Calculate final year savings for annotations 
  final_savings_2028 <- scenarios_clean |>
    filter(year == 2028) |>
    select(scenario, spend_millions)
  
  status_quo_final <- final_savings_2028$spend_millions[final_savings_2028$scenario == "Status Quo"]
  dual_savings <- status_quo_final - final_savings_2028$spend_millions[final_savings_2028$scenario == "Dual Supplier"]
  single_savings <- status_quo_final - final_savings_2028$spend_millions[final_savings_2028$scenario == "Single Supplier"]
  
  
  chart3 <- ggplot(scenarios_clean, aes(x = year, y = spend_millions, color = scenario_type)) +
    
    # Geoms
    geom_vline(
      xintercept = 2024.5, linetype = "dotted",
      color = "#7f8c8d", alpha = 0.8, linewidth = 1, linewidth = 0.5
    ) +
    geom_line(
      data = scenarios_clean |> filter(year <= 2024),
      linewidth = 1.2, alpha = 0.9, linetype = "solid", color = "#34495e"
    ) +
    geom_line(
      data = scenarios_clean |> filter(year >= 2024),
      aes(linetype = scenario_type), linewidth = 1.2, alpha = 0.9
    ) +
    geom_point(
      data = scenarios_clean |> filter(year <= 2024),
      size = 3.5, alpha = 0.9, color = "#34495e"
    ) +
    geom_point(
      data = scenarios_clean |> filter(year > 2024),
      aes(color = scenario_type), size = 3.5, alpha = 0.9
    ) +
    geom_text(
      data = scenarios_clean |> filter(year == 2028),
      aes(label = paste0(scenario, "\n$", round(spend_millions, 1), "M")),
      hjust = -0.15, size = 3.5, fontface = "bold", show.legend = FALSE
    ) +
    
    # Annotate
    annotate("text",
             x = 2023, y = 4.8, label = "ACTUAL",
             size = 3.5, color = "#7f8c8d", fontface = "bold", hjust = 0.5
    ) +
    annotate("text",
             x = 2026.5, y = 4.8, label = "FORECAST",
             size = 3.5, color = "#7f8c8d", fontface = "bold", hjust = 0.5
    ) +
    annotate("text",
             x = 2028, y = 3.7,
             label = paste0("Save $", round(dual_savings, 1), "M\nvs Status Quo"), 
             size = 3.8, color = "#27AE60", fontface = "bold.italic",
             hjust = 0
    ) +
    annotate("text",
             x = 2028, y = 2.5,
             label = paste0("Save $", round(single_savings, 1), "M\nvs Status Quo"), 
             size = 3.8, color = "#3498DB", fontface = "bold.italic",
             hjust = 0
    ) +
    
    # Scales
    scale_color_manual(values = c(
      "Current Path" = "#E74C3C",
      "Recommended" = "#27AE60",
      "Aggressive" = "#3498DB"
    )) +
    scale_linetype_manual(values = c(
      "Current Path" = "dashed",
      "Recommended" = "solid",
      "Aggressive" = "dotted"
    )) +
    scale_x_continuous(breaks = 2022:2028, limits = c(2022, 2029.5)) +
    scale_y_continuous(
      labels = dollar_format(suffix = "M", accuracy = 1),
      limits = c(2, 5.2)
    ) +
    
    # Labs
    labs(
      title = "Strategic Supplier Scenarios: Optimized Mix Saves $1.3M by 2028",
      subtitle = "Dual supplier strategy balances risk and savings vs single supplier approach",
      x = "Year",
      y = "Annual Spend",
      caption = "Actual: Historical data 2022-2024 | Forecast: Status Quo 15% growth, Dual Supplier C+A mix, Single focus on C | Source: Strategic Sourcing Analysis, XYZ Products"
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
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20),
      plot.caption = element_text(size = 9, color = "#95a5a6")
    ) +
    guides(
      color = guide_legend(title = "Strategy"),
      linetype = guide_legend(title = "Strategy")
    )

  return(chart3)
}

