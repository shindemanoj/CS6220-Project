#############################################################################
# Exploring the Kickstarter data to better understand possible project ideas
# Source: https://www.kaggle.com/kemical/kickstarter-projects/data
#
# Interested in understanding outcomes under different competitive
# environments.
#############################################################################
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

ks2016 <- read_csv("ks-projects-201612.csv")
ks2018 <- read_csv("ks-projects-201801.csv")


# note that ks2016 csv file looks corrupted.
# Using just 2018 for now.


# Extract year, month, and day for launch and deadline

ks2018m <- ks2018 %>%
    mutate(
        deadline_ymd = ymd(deadline),
        launched_ymd = ymd_hms(launched)
    ) 


ks2018m <- ks2018m %>%
    mutate(
        deadline_year = year(deadline_ymd),
        deadline_month = month(deadline_ymd),
        deadline_day = day(deadline_ymd),
        launched_year = year(launched_ymd),
        launched_month = month(launched_ymd),
        launched_day = day(launched_ymd)
    ) %>%
    select( -deadline_ymd, -launched_ymd)

# Get number of deadlines by year

ks2018m %>%
    group_by(deadline_year) %>%
    summarize(
        total = n(),
        success_prop = sum(state == 'successful') / n(),
        amount_total = sum(usd_pledged_real),
        amount_prop = sum(usd_pledged_real) / sum(usd_goal_real)
    ) %>%
    arrange(desc(total))

# Get number of deadlines by category
ks2018m <- ks2018m %>%
    mutate(
        usd_pledged_real_total = sum(usd_pledged_real),
        usd_goal_real_total = sum(usd_goal_real)
    )

cat2018 <- ks2018m %>% group_by(main_category) %>%
    summarize(
        total = n(),
        success_prop = sum(state == 'successful') / n(),
        dollars_total = sum(usd_pledged_real)
    ) %>%
    arrange(desc(total))


cat2018

# The deadlines by category look interesting. Let's make a picture.
ggplot(cat2018) +
    geom_col(mapping = aes(x = main_category, y=total, fill=success_prop))


ggplot(cat2018) +
    geom_col(mapping = aes(x = main_category, y=dollars_total, fill=success_prop))


# It seems like digging into probability of funding by category would be interesting.
# For example, the top three most funded categories were also among the least likely
# to reach their funding goals. Meanwhile, dance & theater were among the least funded
# but most likely to reach their funding goals.


