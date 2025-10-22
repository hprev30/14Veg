library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
veg_master = read_csv("https://raw.githubusercontent.com/hprev30/14Veg/main/gtm_turb.csv")
set_master = read_csv("https://raw.githubusercontent.com/hprev30/14Veg/main/set_master.csv")

#getting year extracted and created own column 
set_master$parsed_date <- parse_date_time(set_master$Date, orders = c("mdy", "mdY"))
set_master$Year <- year(set_master$parsed_date)

#getting site codes into the dataset
set_master <- set_master %>%
  mutate(
    site_name = case_when(
      Site == 46 ~ "PC",
      Site == 0  ~ "HI",
      Site == 1  ~ "WO",
      Site == 6  ~ "MC",
      Site == 22 ~ "EC",
      Site == 40 ~ "PI",
      TRUE ~ NA_character_  # or some default value like "Other"
    )
  )

set_master_clean <- set_master %>%
  filter(
    !Obstruction %in% c("ST", "S", "H", "CB", "CH", "H,PUW")     # not in the list                   
  )

set_master <- set_master %>%
  filter(!(site_name == "EC" & Year == "2025" & Station == "B" & Degrees %in% c(25, 115)))

set_master <- set_master %>%
  filter(!(Station %in% c("L", "M", "U")))

unique(set_master_clean$Obstruction)

#average heights 

average_heights <- set_master %>%
  group_by(Year, site_name, Station) %>%
  summarise(avg_height = mean(`Height_Adj (mm)`, na.rm = TRUE)) %>%
  ungroup()

average_heights_c <- set_master_clean %>%
  group_by(Year, site_name, Station) %>%
  summarise(avg_height = mean(`Height_Adj (mm)`, na.rm = TRUE)) %>%
  ungroup()

joined <- left_join(average_heights, average_heights_c, by = c("Year", "site_name", "Station"))

# Subtract columns: e.g., height1 from df1 and height2 from df2
joined <- joined %>%
  mutate(height_diff = avg_height.x - avg_height.y)

a <- ggplot(average_heights_c, aes(x = Year, y = avg_height)) +
  geom_line() +
  facet_grid(Station ~ site_name) +
  geom_vline(data = vline_data, aes(xintercept = xintercept),
             linetype = "dashed", color = "red") +
  labs(
    title = "Average SET Heights Over Time by Station and Site",
    x = "Year",
    y = "Height (mm)"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), labels = function(x) as.integer(x)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

a

vline_data <- data.frame(site_name = c("EC", "WO"), xintercept = 2017)

twenty13wo <- set_master %>%
  filter(Year == 2020 & site_name == "WO")

# Create the dot plot
 b = ggplot(twenty13wo, aes(x = factor(Pin), y = `Height_Adj (mm)`, color = Station)) +
  geom_point() + scale_color_manual(values = station_colors) +
  geom_text(aes(label = Obstruction), vjust = -1, size = 3) +
  labs(
    title = "Height Adjustment with Obstruction Codes",
    x = "Pin Number",
    y = "Height"
  ) +
  theme_minimal() 

 b
station_colors = c("A"= "blue", "B" = "red", "C" = 'green4')
 