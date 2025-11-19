#####install packages----
install.packages('mgcv')
install.packages('purrr')
install.packages('broom')
install.packages('patchwork')
install.packages("officer")
install.packages("flextable")
install.packages("gratia")

#####loading packages-----
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(mgcv)
library(purrr)
library(broom)
library(tidyr)
library(patchwork)
library(officer)
library(flextable)
library(gratia)

#####loading in datasets-----

#set master
set_master = read_csv("https://raw.githubusercontent.com/hprev30/14Veg/main/set_master.csv")

#elevation of set bench marks for conversion to true elevation
bmelev = read_csv("https://raw.githubusercontent.com/hprev30/14Veg/main/benchmark_elevations.csv")

#vegetation master
#####adding more identifiers to dataset for betting subsetting----

#adding year and month columns to master
set_master$parsed_date <- parse_date_time(set_master$Date, orders = c("mdy", "mdY"))
set_master$Year <- year(set_master$parsed_date)
set_master$Month <- month(set_master$parsed_date, label = TRUE, abbr = FALSE)

#adding site codes to dataset
set_master <- set_master %>%
  mutate(
    Site = case_when(
      Site == 46 ~ "PC",
      Site == 0  ~ "HI",
      Site == 1  ~ "WO",
      Site == 6  ~ "MC",
      Site == 22 ~ "EC",
      Site == 40 ~ "PI",
      TRUE ~ NA_character_  # or some default value like "Other"
    )
  )

#adding season column to the dataset
set_master$Date <- as.Date(set_master$Date, format = "%m/%d/%Y")
# If conversion gives NAs, try short year format
if (any(is.na(set_master$Date))) {
  set_master$Date <- as.Date(set_master$Date, format = "%m/%d/%y")
}
set_master$Month <- as.integer(format(set_master$Date, "%m"))

# Assign season based on month
set_master$season <- with(set_master, ifelse(Month %in% c(12, 1, 2), "Winter",
                                             ifelse(Month %in% 3:5, "Spring",
                                                    ifelse(Month %in% 6:8, "Summer",
                                                           "Fall"))))

#####cleaning dataset----

#clearing out when data from an unlevel SET

merged_set <- merged_set %>%
  filter(!(Site == "EC" & Year == "2025" & Station == "B" & Degrees %in% c(25, 115)))

#clearing out WLWL data
merged_set <- merged_set %>%
  filter(!(Station %in% c("L", "M", "U")))

#clearing out datapoints from veg, shells, and crab structures
set_master_clean <- set_master %>%
  filter(
    !Obstruction %in% c("ST", "S", "CB", "CH")     # not in the list                   
  )

unique(merged_set$Obstruction)
unique(merged_set$Station)

#averaging spring and fall months from 2014
d2014a = fall.spring2014 %>%
  group_by(Site, Station, Degrees, Pin, season) %>%
  summarise(`Height_Adj (mm)` = mean(`Height_Adj (mm)`))

#####subsetting data-----

#subsetting to only fall and spring data included. No 2013 data and only fall and spring of 2014
fall.spring <- set_master_clean %>%
  mutate(
    Year = as.integer(format(Date, "%Y")),
    Month = as.integer(format(Date, "%m"))
  ) %>%
  filter(
    # Keep only Apr, May, Sep, Oct from 2014
    (Year == 2014 & Month %in% c(4, 5, 9, 10)) |
      # Keep all years except 2013 and 2014
      (Year != 2013 & Year != 2014)
  ) 

#subsetting just 2014 and 2015-2025
fall.spring2014 <- fall.spring %>%
  mutate(Year = as.integer(format(Date, "%Y"))) %>%  # extract year if not already present
  filter(Year == 2014)

fall.spring15.25 <- fall.spring %>%
  mutate(Year = as.integer(format(Date, "%Y"))) %>%  # extract year if not already present
  filter(Year != 2014)

#merging clean 2014 data and the rest of the data

merged_set <- rbind(d2014a, fall.spring15.25)

merged_set <- merged_set %>%
  mutate(Year = ifelse(is.na(Year), 2014, Year))

#fall subset 
fall_set_final <- merged_set %>%
  filter(season == "Fall")

#####colors for visuals-----

station_colors = c("A"= "blue", "B" = "red", "C" = 'green4')
site_colors = c("WO" = "blue", "EC" = 'cyan3', "HI" = 'red', "MC" = 'green4', "PI" = "purple", "PC" = "black")

#####converting to true elevation----
voffset = 379

true_elev <- bmelev %>%
  left_join(fall_set_final, by = c("Site", "Station")) %>%
  mutate(true_elevation = Elevation + voffset - `Height_Adj (mm)`)

#####taking yearly averages of each site and station------
average_heights <- true_elev2 %>%
  group_by(Year, Site, Station) %>%
  summarise(avg_height = mean(elev_diff, na.rm = TRUE),
            se_height = sd(elev_diff, na.rm = TRUE) / sqrt(sum(!is.na(true_elevation)))) %>%
  ungroup()

true_elev2 <- true_elev %>%
  group_by(Site, Station, Degrees, Pin) %>%
  mutate(
    elev_2014 = true_elevation[Year == 2014][1],   # 2014 value for this Station/Pin
    elev_diff = elev_2014 - true_elevation       # difference from 2014
  ) %>%
  ungroup()


#####subsetting the yearly averages by site to prepare for GAMs----
average_heights$Station <- as.factor(average_heights$Station)
wo <- average_heights %>%
  filter(trimws(Site) == "WO")
pi <- average_heights %>%
  filter(trimws(Site) == "PI")
hi <- average_heights %>%
  filter(trimws(Site) == "HI")
ec <- average_heights %>%
  filter(trimws(Site) == "EC")
pc <- average_heights %>%
  filter(trimws(Site) == "PC")
mc <- average_heights %>%
  filter(trimws(Site) == "MC")

######fitting GAMs------

#washington oaks
gam_wo <- gam(avg_height ~ s(Year, k = 3) + s(Station, bs = "re"), data = wo)
gam_wo2 <- gam(avg_height ~ s(Year, k=3), data = wo)
lm_wo = lm(avg_height ~ Year + Station, data = wo)

AIC(gam_wo, gam_wo2, lm_wo)

gam.check(gam_wo)
summary(gam_wo)

re <- smooth_estimates(gam_wo)  # from 'gratia' package
head(re)
plot(gam_wo, select = 2, shade = TRUE, main = "Station random effects")
qqnorm(resid(gam_wo)); qqline(resid(gam_wo))

#pellicer creek
gam_pc <- gam(avg_height ~ s(Year, k = 3) + s(Station, bs = "re"), data = pc)
gam_pc2 <- gam(avg_height ~ s(Year, k=3), data = pc)
lm_pc = lm(avg_height ~ Year + Station, data = pc)

AIC(gam_pc, gam_pc2, lm_pc)
summary(gam_pc)
gam.check(gam_pc)


re <- smooth_estimates(gam_pc)  # from 'gratia' package
head(re)
plot(gam_pc, select = 2, shade = TRUE, main = "Station random effects")
qqnorm(resid(gam_pc)); qqline(resid(gam_pc))

#hat island
gam_hi <- gam(avg_height ~ s(Year, k = 3) + s(Station, bs = "re"), data = hi)
gam_hi2 <- gam(avg_height ~ s(Year, k=3), data = hi)
lm_hi = lm(avg_height ~ Year + Station, data = hi)

AIC(gam_hi, gam_hi2, lm_hi)
summary(gam_hi)
gam.check(gam_hi)

re <- smooth_estimates(gam_hi)  # from 'gratia' package
head(re)
plot(gam_hi, select = 2, shade = TRUE, main = "Station random effects")
qqnorm(resid(gam_hi)); qqline(resid(gam_hi))

hi.plot <- ggplot(average_heights, aes(x = Year, y = avg_height, color = Station)) +
  geom_point(aes(color = Station), size = 3) +    # points colored by Station
  geom_smooth(method = "gam", formula = y ~ s(x, k=3), se = TRUE, color = "black") + # one GAM
  scale_color_manual(values = station_colors) +        
  scale_x_continuous(breaks = seq(min(average_heights$Year), max(average_heights$Year), by = 1)) + 
  labs(title = "Hat Island Annual Average Elevation Change",      
       x = "Year", 
       y = "Elevation (mm)") +
  theme_minimal(base_size = 14) + facet_wrap( ~Site) +                
  theme(
    panel.grid = element_blank(),                 # remove grid
    plot.title = element_text(hjust = 0.5),      # center title
    axis.line = element_line(color = "black"),   # add axes lines
    axis.ticks = element_line(color = "black")   # optional: black tick marks
  )

hi.plot 

#east creek
gam_ec <- gam(avg_height ~ s(Year, k = 3) + s(Station, bs = "re"), data = ec)
gam_ec2 <- gam(avg_height ~ s(Year, k=3), data = ec)
lm_ec = lm(avg_height ~ Year + Station, data = ec)

AIC(gam_ec, gam_ec2, lm_ec)
summary(gam_ec)
gam.check(gam_ec)

re <- smooth_estimates(gam_ec)  # from 'gratia' package
head(re)
plot(gam_ec, select = 2, shade = TRUE, main = "Station random effects")
qqnorm(resid(gam_ec)); qqline(resid(gam_ec))

ec.plot <- ggplot(ec, aes(x = Year, y = avg_height, color = Station)) +
  geom_point(aes(color = Station), size = 3) +    # points colored by Station
  geom_smooth(method = "gam", formula = y ~ s(x, k=3), se = TRUE, color = "black") + # one GAM
  scale_color_manual(values = station_colors) +        
  scale_x_continuous(breaks = seq(min(ec$Year), max(ec$Year), by = 1)) + 
  labs(title = "East Creek Annual Average Elevation Change",      
       x = "Year", 
       y = "Elevation (mm)") +
  theme_minimal(base_size = 14) +                 
  theme(
    panel.grid = element_blank(),                 # remove grid
    plot.title = element_text(hjust = 0.5),      # center title
    axis.line = element_line(color = "black"),   # add axes lines
    axis.ticks = element_line(color = "black")   # optional: black tick marks
  )

ec.plot

#pine island

gam_pi <- gam(avg_height ~ s(Year, k = 3) + s(Station, bs = "re"), data = pi)
gam_pi2 <- gam(avg_height ~ s(Year, k=3), data = pi)
lm_pi = lm(avg_height ~ Year + Station, data = pi)

AIC(gam_pi, gam_pi2, lm_pi)
summary(gam_pi)
gam.check(gam_pi)

re <- smooth_estimates(gam_pi)  # from 'gratia' package
head(re)
plot(gam_pi, select = 2, shade = TRUE, main = "Station random effects")
qqnorm(resid(gam_pi)); qqline(resid(gam_pi))

#moses creek

gam_mc <- gam(avg_height ~ s(Year, k = 3) + s(Station, bs = "re"), data = mc)
gam_mc2 <- gam(avg_height ~ s(Year, k=3), data = mc)
lm_mc = lm(avg_height ~ Year + Station, data = mc)

AIC(gam_mc, gam_mc2, lm_mc)
summary(gam_mc)
gam.check(gam_mc)

re <- smooth_estimates(gam_mc)  # from 'gratia' package
head(re)
plot(gam_mc, select = 2, shade = TRUE, main = "Station random effects")
qqnorm(resid(gam_mc)); qqline(resid(gam_mc))

