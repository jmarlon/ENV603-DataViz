#### ENV 603 / 5-April-2021 / N.R. Sommer
# Dataset 1: Religion by region

# If you have not yet installed these libraries, use install.package("")

library(tidyverse)
library(socviz)
library(RColorBrewer)

# Create a new table called rel_by_region

rel_by_region <- gss_sm %>%
  group_by(bigregion, religion) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))

# See how the pipeline above has taked the gss_sm dataframe and transformed it into a summary table.

View(gss_sm)
View(rel_by_region)

# Now let's make some plots!

p1 <- ggplot(rel_by_region, aes(x = bigregion, y = pct, fill = religion)) + 
  geom_col(position = "dodge2") +
  labs(x = "Region",y = "Percent", fill = "Religion") +
  theme(legend.position = "top")

p1

p2 <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion)

p2
ggsave("plot1_try1.png",
       plot = last_plot())

# Make modifications to either plot. Google is your friend here. A few suggestions:
# (1) Add a title
# (2) Remove the gridlines
# (3) Reorder the bars
# (4) Choose a new color scheme

# Combine NA + Other + None 

rel_by_region <- rel_by_region %>%
    mutate(religion.group = case_when(
      religion == "None" ~ "Other",
      religion == "Other" ~ "Other",
      is.na(religion) ~ "Other",
      TRUE ~ as.character(religion)
    ))


p3 <- ggplot(rel_by_region, aes(x = religion.group, y = pct, fill = religion.group)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion)
 
p3 # WRONG
ggsave("plot2_case_when_sequencing_mistake.png",
       plot = last_plot())

#--> Need to recode first, THEN calculate pct

rel_by_region2 <- gss_sm %>% 
  mutate(religion.group = case_when(
  religion == "None" ~ "Other",
  religion == "Other" ~ "Other",
  is.na(religion) ~ "Other",
  TRUE ~ as.character(religion))) %>% 
  group_by(bigregion, religion.group) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round((freq*100), 0))


p4 <- ggplot(rel_by_region2, aes(x = religion.group, y = pct, fill = religion.group)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion)

p4 # CORRECT
ggsave("plot3_correct_grouping_and_percentages.png",
       plot = last_plot())

# Add title, clean up gridlines, etc.
# Help is here: https://www.datanovia.com/en/blog/ggplot-theme-background-color-and-grids/

p5 <- ggplot(rel_by_region2, aes(x = religion.group, y = pct, fill = religion.group)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  guides(fill = FALSE) + 
  coord_flip() + 
  facet_grid(~ bigregion) +
  ggtitle("Protestants are most prevalent in the South, Catholics in the Northeast") +
  theme_bw() +
  theme(
    
#    plot.background = element_rect(),    # Background of the entire plot
    
#    panel.background = element_rect(),   # Background of plotting area
#    panel.border = element_rect(),       # Border around plotting area.
    # fill argument should be NA
    
#    panel.grid = element_line(),         # All grid lines
#    panel.grid.major = element_line(),   # Major grid lines
    panel.grid.minor = element_blank(),   # Minor grid lines
    
#    panel.grid.major.x = element_line(), # Vertical major grid lines
    panel.grid.major.y = element_blank(), # Horizontal major grid lines
#    panel.grid.minor.x = element_line(), # Vertical minor grid lines
#    panel.grid.minor.y = element_line()  # Vertical major grid lines
  )

p5
p5 +  scale_y_continuous(breaks=c(0, 25, 50))
ggsave("plot4_theme_cleaning.png",
       plot = last_plot())

## Change the ordering of the religions (i.e., relevel the factor)

levels(rel_by_region2$religion.group) # check the levels... it's not a factor!
rel_by_region2$religion.group <- factor(rel_by_region2$religion.group) # make it a factor
levels(rel_by_region2$religion.group) # check the levels... NOW it's a factor

# reorder the levels
rel_by_region2$religion.group <- fct_relevel(rel_by_region2$religion.group, "Other", "Jewish", "Catholic")
levels(rel_by_region2$religion.group) # check the new levels

p6 <- ggplot(rel_by_region2, aes(x = religion.group, y = pct, fill = religion.group)) +
  geom_col(position = "dodge2") +
  labs(x = NULL, y = "Percent", fill = "Religion") +
  guides(fill = FALSE) + 
  scale_y_continuous(breaks=c(0, 25, 50)) +
  scale_fill_brewer(palette = "Pastel1") + 
  coord_flip() + 
  facet_grid(~ bigregion) +
  ggtitle("Protestants are most prevalent in the South, Catholics in the Northeast") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),   # Minor grid lines
    panel.grid.major.y = element_blank(), # Horizontal major grid lines
  )

p6


# Once you're happy with your changes, save your plot:
ggsave("plot5_reorder_factor_levels.png",
  plot = last_plot(),
  dpi = 150)
