# load in packages
library(tidyr)
library(dplyr)
library(countrycode)
library(ggplot2)
library(stringr)

# load in data
imports <- read.csv("imports_combined.csv")

# add column for month
# grab all years included in data
sort(unique(imports$YEAR))
imports <- mutate(.data = imports, MONTH_NUMBER = (YEAR - 2012) * 12 + MONTH) %>%
# filter out 2010 and 2011
  filter(YEAR >= 2012) 

# fresh vs frozen
tmp <- imports %>%
  filter(AIR_WGT_MO > 1000) %>%
  mutate(FRESH_FROZEN = case_when(
    str_detect(string = I_COMMODITY, pattern = "^301|^302") ~ "fresh", 
    str_detect(string = I_COMMODITY, pattern = "^303") ~ "frozen"))
                                


# identify countries with iso3c codes
countries <- imports %>%
  select(CTY_CODE, CTY_NAME) %>%
  distinct() %>%
  mutate(iso3c = countrycode(sourcevar = CTY_NAME, origin = "country.name.en", destination = "iso3c"))
# manually confirmed countries without code were groups and not true countries (e.g. NATO, European Union, etc)

# remove rows without iso3c codes
true_countries <- countries %>% 
  filter(!is.na(iso3c)) 

# set up data for analysis 
air_over_time <- imports %>%
  # filter by countries with iso3c codes to leave only true countries
  filter(CTY_CODE %in% true_countries$CTY_CODE) %>% 
  # group data by month 
  group_by(MONTH_NUMBER) %>% 
  # calculate air and vessel totals by month
  summarise(AIR_TOTAL = sum(AIR_WGT_MO), 
            VESSEL_TOTAL = sum(VES_WGT_MO)) %>%
  # create column for proportion of total imports 
  mutate(PROP_AIR = AIR_TOTAL / (AIR_TOTAL + VESSEL_TOTAL)) 
    # not including containerized vessels due to data largely being the same numbers as vessels, will need to re-pull

## TOTAL AIR IMPORTS (KG) OVER TIME

# plot total air imports over time
ggplot(data = air_over_time, mapping = aes(x = MONTH_NUMBER, y = AIR_TOTAL)) + 
  geom_point() + labs(x = "Time (2012-2024)", y = "Seafood Imported by Air (Kg)", title = "Total Seafood Imported to the US by Air 2012-2024")

# create a model for air imports over time
m1 <- lm(AIR_TOTAL ~ MONTH_NUMBER, data = air_over_time)
summary(m1)

# establish residuals
e1 <- resid(m1)
# plot residuals against model predictions
plot(fitted(m1), e1, pch = 16, las = 1, xpd = NA,
     cex.lab = 1.5, xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals")

# produce a QQ plot to see if residuals are normally distributed
qqnorm(e1, pch =16)
qqline(e1)

# create a null model for an F test
null_m1 <- lm(AIR_TOTAL ~ 1, data = air_over_time)
# conduct F test
anova(null_m1, m1)

## PROPORTION AIR IMPORTS OVER TIME

# plot proportion air imports over time
ggplot(data = air_over_time, mapping = aes(x = MONTH_NUMBER, y = PROP_AIR)) + 
  geom_point() + labs(x = "Time (2012-2024)", y = "Proportion of Seafood Imported by Air (Kg)", title = "Proportion of Seafood Imported to the US by Air 2012-2024")

# model proportion air imports over time
m2 <- lm(PROP_AIR ~ MONTH_NUMBER, data = air_over_time)
summary(m2)

# create a null model for an F test
null_m2 <- lm(PROP_AIR ~ 1, data = air_over_time)
# conduct an F test
anova(null_m2, m2)