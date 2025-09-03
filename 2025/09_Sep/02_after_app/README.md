# Makeover Dashboard

*Storytelling with Data Challenge — Sep 2025 \| Dashboards that Deliver*

## Plan

### 0. App

-   <https://0l6jpd-steven-ponce.shinyapps.io/02_after_app/>

### 1. Design brief

-   **Audience:** Transport planners / local policy staff.

-   **Key questions:**

1.  How is **daily volume** trending this month?

2.  What’s the **weekday vs weekend** hourly profile?

3.  Which **sensor** is consistently faster/slower?

-   **Success:** 3 KPIs + 2 focused charts answer the questions at a glance.

### 2. App choices

-   **One page** with a small sidebar.

-   **3 controls only:** date range, sensor (single/multi), weekday/weekend toggle.

-   **3 KPIs:** Avg daily volume, Median speed, % large vehicles.

-   **2 charts:** (A) Daily total volume (main) with smoothed trend; (B) Weekday vs Weekend hourly profile.

### 3. Data Source

-   The `National Highways Traffic Flow` data comes from the 2024 TidyTuesday 49 dataset via the `WebTRIS Traffic Flow API.`

-   Data Link: <https://github.com/rfordatascience/tidytuesday/tree/main/data/2024/2024-12-03>

-   National Highways operates and maintains motorways and major A roads in England. They directly monitor the speed and flow of roads using on-road sensors, and the data can be accessed via the [National Highways API](https://webtris.nationalhighways.co.uk/api/swagger/ui/index).

-   The data has vehicle size and speed information for **May 2021** from four different road sensors on the **A64 road**.

### 4.Data Dictionary

### `A64_traffic.csv`

| variable | class | description |
|:------------------|:------------------|:-----------------------------------|
| SiteId | character | Road sensor ID number. |
| Site Name | character | Name of road sensor, also often a number. |
| Report Date | character | Date that the data was recorded. |
| Time Period Ending | character | Time of day in hh:mm:ss that time period ends. |
| Time Interval | double | Number of the time interval during the day. |
| 0 - 520 cm | double | Number of vehicles between 0 and 520cm. |
| 521 - 660 cm | double | Number of vehicles between 521 and 660cm. |
| 661 - 1160 cm | double | Number of vehicles between 661 and 1160cm. |
| 1160+ cm | double | Number of vehicles over 1160cm. |
| 0 - 10 mph | double | Number of vehicles travelling between 0 and 10mph. |
| 11 - 15 mph | double | Number of vehicles travelling between 11 and 10mph. |
| 16 - 20 mph | double | Number of vehicles travelling between 16 and 20mph. |
| 21 - 25 mph | double | Number of vehicles travelling between 21 and 25mph. |
| 26 - 30 mph | double | Number of vehicles travelling between 26 and 30mph. |
| 31 - 35 mph | double | Number of vehicles travelling between 31 and 35mph. |
| 36 - 40 mph | double | Number of vehicles travelling between 36 and 40mph. |
| 41 - 45 mph | double | Number of vehicles travelling between 41 and 45mph. |
| 46 - 50 mph | double | Number of vehicles travelling between 46 and 50mph. |
| 51 - 55 mph | double | Number of vehicles travelling between 51 and 55mph. |
| 56 - 60 mph | double | Number of vehicles travelling between 56 and 60mph. |
| 61 - 70 mph | double | Number of vehicles travelling between 61 and 70mph. |
| 71 - 80 mph | double | Number of vehicles travelling between 71 and 80mph. |
| 80+ mph | double | Number of vehicles travelling over 80mph. |
| Avg mph | double | Average speed of all vehicles in miles per hour. |
| Total Volume | double | Total number of vehicles. |
| Name | character | Name of sensor site describing location. |
| Longitude | double | Longitude of road sensor. |
| Latitude | double | Latitude of road sensor. |
| Status | character | Whether the road sensor is active or inactive. |
