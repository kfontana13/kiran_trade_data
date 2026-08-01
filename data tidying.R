# load in packages
library(tidyr)
library(dplyr)
library(countrycode)
library(ggplot2)

# load in data
imports <- read.csv("imports_combined.csv")

# add column for month
# grab all years included in data
sort(unique(imports$YEAR))
imports <- mutate(.data = imports, MONTH_NUMBER = (YEAR - 2012) * 12 + MONTH) %>%
# filter out 2010 and 2011
  filter(YEAR >= 2012)

# identify countries with iso3c codes
countries <- imports %>%
  select(CTY_CODE, CTY_NAME) %>%
  distinct() %>%
  mutate(iso3c = countrycode(sourcevar = CTY_NAME, origin = "country.name.en", destination = "iso3c"))
# manually confirmed countries without code were not true countries (e.g. NATO, European Union, etc)

# remove rows without iso3c codes
true_countries <- countries %>% 
  filter(!is.na(iso3c)) 

# filter out country groups to leave only true countries
air_over_time <- imports %>%
  filter(CTY_CODE %in% true_countries$CTY_CODE) %>% # filter by countries with iso3c codes
  group_by(MONTH_NUMBER) %>% 
  summarise(AIR_TOTAL = sum(AIR_WGT_MO), # calculate air and vessel totals by month
            VESSEL_TOTAL = sum(VES_WGT_MO)) %>%
  mutate(PROP_AIR = AIR_TOTAL / (AIR_TOTAL + VESSEL_TOTAL)) # create column for proportion of total imports 
    # not including containerized vessels due to data largely being the same numbers as vessels, will need to re-pull

## TOTAL AIR IMPORTS (KG) OVER TIME

# plot total air imports over time
ggplot(data = air_over_time, mapping = aes(x = MONTH_NUMBER, y = AIR_TOTAL)) + 
  geom_point() + labs(x = "Time (2012-2024)", y = "Seafood Imported by Air (Kg)", title = "Total Seafood Imported to the US by Air 2012-2024")

# model air imports over time
m1 <- lm(AIR_TOTAL ~ MONTH_NUMBER, data = air_over_time)
summary(m1)

# establish residuals
e1 <- resid(m1)
# plot residuals against model predictions
plot(fitted(m1), e1, pch = 16, las = 1, xpd = NA,
     cex.lab = 1.5, xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals")

# produce a qq plot to see if residuals are normally distributed
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

str_detect(code, pattern = ???)

