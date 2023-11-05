


## helper functions


# summarize only ----
summary_only <- function(tbl) {
  
  tbl %>% 
    
    # summary
    summarise(
      median_award     = median(award),
      median_recipient = median(recipient),
      .groups = "drop",
    ) %>% 
    
    mutate(median_award_per_recipient = median_award / median_recipient) %>%
    arrange(year) 
}


# group by and then summarize ----
group_summary <- function(tbl, group_col) {
  
  tbl %>% 
    
    # summary
    group_by( {{ group_col }}) %>% 
    summarise(
      median_award     = median(award),
      median_recipient = median(recipient),
      .groups          = "drop",
    ) %>% 
    
    mutate(median_award_per_recipient = median_award / median_recipient) %>% 
    
    arrange(year)

}


