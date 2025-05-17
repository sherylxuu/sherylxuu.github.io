
library(tidyverse)
oh <- read.csv("order_header_sample.csv")
od <- read.csv("order_detail_sample2.csv")
ss <- read.csv("session_sample.csv")
cs <- read.csv("customer_sample.csv")
lu <- read.csv("lookup.csv")

library(showtext)
font_add_google("Noto Serif", "noto_serif")
showtext_auto()
library(scales)     
library(lubridate) 


#1 CODE
oh$dlv_dt2 <- as.Date(oh$dlv_dt, '%Y-%m-%d')
str(oh)
summary(oh)

ord <- oh %>%
  inner_join(od, by = "ord_id")

monsales <- ord %>%
  mutate(month = month(dlv_dt2)) %>%
  group_by(month) %>%
  summarize(totsales = sum(tot_pr_qy,  na.rm=TRUE)) %>%
  arrange(month)
monsales

#1 VISUALIZATION
oh$dlv_dt2 <- as.Date(oh$dlv_dt, '%Y-%m-%d')

ord <- oh %>%
  inner_join(od, by = "ord_id")

monsales <- ord %>%
  mutate(
    month = month(dlv_dt2),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall"
    )
  ) %>%
  group_by(month, season) %>%
  summarize(totsales = sum(tot_pr_qy, na.rm = TRUE)) %>%
  arrange(month) %>%
  mutate(month = factor(month, levels = 1:12,
                        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

season_colors <- c("Winter" = "#9CC3D5", 
                   "Spring" = "#FFD700", 
                   "Summer" = "#FFA07A", 
                   "Fall"   = "#CD5C5C")

ggplot(monsales, aes(x = month, y = totsales, group = 1)) +
  geom_rect(
    aes(
      xmin = as.numeric(month) - 0.5, 
      xmax = as.numeric(month) + 0.5, 
      ymin = -Inf, 
      ymax = Inf, 
      fill = season
    ), 
    alpha = 0.5
  ) +
  geom_line(color = "#4E2A84", size = 1.8) +
  geom_point(color = "#5091CD", size = 4, shape = 21, fill = "white", stroke = 1.5) +
  scale_fill_manual(values = season_colors) +
  labs(
    title = "Total Sales by Month with Seasonal Divisions", 
    x = "Month", 
    y = "Total Sales", 
    fill = "Season"
  ) +
  theme_minimal(base_family = "noto_serif") +
  theme(
    plot.background = element_rect(fill = "#E4E0EE", color = NA),
    panel.background = element_rect(fill = "#E4E0EE", color = NA),
    panel.grid.major = element_line(color = "white", linetype = "solid"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#4E2A84", size = 12),
    axis.title = element_text(color = "#4E2A84", size = 14, face = "bold"),
    plot.title = element_text(color = "#4E2A84", size = 18, face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.background = element_rect(fill = "#E4E0EE", color = NA),
    legend.key = element_rect(fill = "white")
  )


#2 CODE
ludet <- lu %>%
  distinct(minor_cat)


cat <- lu %>%
  mutate(category = ifelse(grepl("GROC SNACKS|GROC BEVERAGES", minor_cat) == TRUE, "Grocery Snacks & Beverages",
                                 ifelse(grepl("GROC", minor_cat) == TRUE, "Other Grocery",
                                        ifelse(grepl("HB", minor_cat) == TRUE, "Health & Beauty",
                                               ifelse(grepl("HH", minor_cat) == TRUE, "Household",
                                                      ifelse(grepl("DAIRY", minor_cat) == TRUE, "Dairy",
                                                             ifelse(grepl("FROZEN", minor_cat) == TRUE, "Frozen",
                                                                    ifelse(grepl("FRESH MARKET", minor_cat) == TRUE, "Fresh Market",
                                                                           ifelse(grepl("HG", minor_cat) == TRUE, "Home Goods", "Others")))))))))


order <- ord %>%
  inner_join(cat, by = "pod_id") %>%
  group_by(category) %>%
  summarize(tot_sales = sum(tot_pr_qy)) 
order


#2 VISUALIZATION

ggplot(order, aes(x = reorder(category, tot_sales), y = tot_sales)) +
  geom_col(width = 0.6, fill = "#4E2A84") +
  
  coord_flip() +
  
  geom_text(aes(label = comma(tot_sales)),
            color = "#4E2A84",   
            hjust = -0.1,        
            size = 4) +
  
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Total Sales by Product Category",
    x = NULL,              
    y = "Total Sales"
  ) +
  
  theme_minimal(base_family = "noto_serif") +
  theme(
    plot.background = element_rect(fill = "#E4E0EE", color = NA),
    panel.background = element_rect(fill = "#E4E0EE", color = NA),
    panel.grid.major.y = element_blank(),      
    panel.grid.major.x = element_line(color = "white"),  
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "#4E2A84", size = 12),
    axis.text.y = element_text(color = "#4E2A84", size = 12),
    axis.title.x = element_text(color = "#4E2A84", size = 14, face = "bold"),
    plot.title = element_text(
      color = "#4E2A84", size = 18, face = "bold", hjust = 0.5
    )
  )


#3 CODE
oh$dlv_dt2 <- as.Date(oh$dlv_dt, '%Y-%m-%d')
str(oh)
summary(oh)

order1 <- od %>%
  inner_join(oh, by = "ord_id")
order1

order2 <- order1 %>%
  inner_join(cat, by = "pod_id")
order2

order3 <- order2 %>%
  mutate(quarter = ifelse(month(dlv_dt2) %in% c(1, 2, 3), "Quarter 1",
                          ifelse(month(dlv_dt2) %in% c(4, 5, 6), "Quarter 2",
                                 ifelse(month(dlv_dt2) %in% c(7, 8, 9), "Quarter 3", "Quarter 4")))) %>%
  group_by(quarter, category) %>%
  summarize(tot_sales = sum(tot_pr_qy))

print(order3, n=36)



#3 VISUALIZATION
order3 <- order3 %>%
  mutate(quarter = factor(quarter, 
                          levels = c("Quarter 1", "Quarter 2", 
                                     "Quarter 3", "Quarter 4")))

ggplot(order3, aes(x = quarter, y = tot_sales, fill = category)) +
  geom_col(position = "stack", width = 0.7, color = "#4E2A84") +
  
  scale_y_continuous(labels = comma) +
  
  scale_fill_brewer(palette = "Set3") +
  
  labs(
    title = "Total Sales by Quarter and Category",
    x = NULL,
    y = "Total Sales",
    fill = "Category"
  ) +
  theme_minimal(base_family = "noto_serif") +
  theme(
    plot.background = element_rect(fill = "#E4E0EE", color = NA),
    panel.background = element_rect(fill = "#E4E0EE", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    
    axis.text = element_text(color = "#4E2A84"),
    axis.title = element_text(color = "#4E2A84", face = "bold"),
    plot.title = element_text(color = "#4E2A84", size = 18, face = "bold", hjust = 0.5),
    
    legend.position = "top",
    legend.title = element_text(size = 14, face = "bold", color = "#4E2A84"),
    legend.text = element_text(size = 12, color = "#4E2A84"),
    legend.background = element_rect(fill = "#E4E0EE", color = NA),
    legend.key = element_rect(fill = "white")
  )


