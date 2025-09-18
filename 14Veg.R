library(readr)

hit = read_csv("https://raw.githubusercontent.com/hprev30/14Veg/main/HI_turb.csv")
mct = read_csv("https://raw.githubusercontent.com/hprev30/14Veg/main/MC_turb.csv")
wot = read_csv("https://raw.githubusercontent.com/hprev30/14Veg/main/WO_turb.csv")
gtmt = read_csv("https://raw.githubusercontent.com/hprev30/14Veg/main/gtm_turb.csv")


pct = subset(gtmt, `Station Code` == 'gtmpcnut')
pit = subset(gtmt, `Station Code` == 'gtmpinut')
ect = subset(gtmt, `Station Code` == 'gtmssnut')

# datasets with **t are turbidity datasets

pct_clean <- subset(pct, !grepl("<-(1|2|3)>", F_TSS))
mean(pct_clean$TSS)
