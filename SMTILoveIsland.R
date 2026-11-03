library(tidyverse)
library(ggplot2)

# import my sheets dataset into this
contestants_usa <- read.csv("C:/Users/rmcar/Projects/SMTI/LoveIslandUSAbios.csv")

# only uncomment if you want to check your data
  # glimpse(contestants_usa)

# separate into Men (column stacks) & Women (row bands)
men <- contestants_usa %>% filter(gender == "M")
women <- contestants_usa %>% filter(gender == "F")

# compute mutal compatibility utility matrix
# U(Man_i, Woman_j)
grid_usa <- expand_grid(man_id = men$id, woman_id = women$id) %>%
  left_join(men, by = c("man_id" = "id")) %>%
  left_join(women, by = c("woman_id" = "id"), suffix = c("_m", "_w")) %>%
  mutate(
    age_diff = abs(age_m - age_w),
    same_region = (hometown_region_m == hometown_region_w),
    
    # utility function formula
    utility_score = 10 - (1.1*age_diff) + (1.5*as.numeric(same_region)) - (0.8*abs(entry_day_m-entry_day_w)),
    
    # apply SMTI suoku rules
      # low compatibility (<5.0) becomes "Blackout" cells (dumped at firepit)
    cell_type = case_when(
      utility_score < 5.0 ~ "Blackout (Dumped)",
      utility_score >= 8.0 ~ "Couple Goals",
      TRUE ~ "Acceptable"
    )
  )

# render partial sudoku SMTI grid
ggplot(grid_usa, aes(x = name_m, y= name_w, fill = cell_type)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(aes(label = round(utility_score, 1)), color = "black", fontface = "bold", size =5) +
  scale_fill_manual(values = c(
    "Blackout (Dumped)" = "black",
    "Acceptable" = "white",
    "Couple Goals" = "#ff69b4"
  )) +
  theme_minimal() +
  labs(
    title = "Love Island USA: Partial Sudoku SMTI Grid",
    x = "Men",
    y = "Women",
    fill = "Grid Status"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11, face = "bold")
  )
  
