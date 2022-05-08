library(tidyverse)
library(naniar)
library(visdat)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(GGally)
# library(corrr)
library(dplyr)
library(car)
library(mgcv)
library(ggfortify)


data = read.csv('data/fatal-police-shootings-data.csv', na.strings=c("","NA"))

# simplify armed feature
data$armed = data$armed %>% replace_na('undetermined')


for (i in 1:length(data$armed)){
    if (data$armed[i] == 'gun') {next}
    else if (data$armed[i] == 'knife') {next}
    else if (data$armed[i] == 'vehicle') {next}
    else if (data$armed[i] == 'unarmed') {next}
    else if (data$armed[i] == 'undetermined') {next}
    else {data$armed[i] = 'other'}
} 

# Convert features to factor and numeric as appropriate
# CHECK: armed, city, date, longitude, latitude

features_to_drop = c("id", "name", "city")
factor_cols = c("manner_of_death", "armed", "gender", "race", "state", "signs_of_mental_illness", "threat_level", "flee", "body_camera")
numeric_cols = c("age")

data = data[,!(names(data) %in% features_to_drop)]
data <- data %>% 
    mutate(across(.cols=all_of(factor_cols), .fns = as.factor))
data <- data %>% 
    mutate(across(.cols=all_of(numeric_cols), .fns = as.numeric))

# convert date from character to date format
data <- data %>% 
    mutate(across(.cols="date", .fns = as_date))

attach(data)

# some bivariate EDA

# MI with gender - higher proportion of women classified as MI
MI_gender = table(signs_of_mental_illness, gender)
MI_gender
prop.table(MI_gender, margin = 2)

# MI with race - black lowest at .155, white highest at 0.289
MI_race = table(signs_of_mental_illness, race)
MI_race
prop.table(MI_race, margin = 2)

# MI with manner_of_death - higher shot and tasered
MI_MoD= table(signs_of_mental_illness, manner_of_death)
MI_MoD
prop.table(MI_MoD, margin = 2)

# MI with state - KY lowest at 0.09, NH and VT highest at 0.45 (though very few cases)
MI_state = table(signs_of_mental_illness, state)
MI_state
prop.table(MI_state, margin = 2)

# MI with threat_level - undetermined much lower than others, prob not helpful
MI_threat = table(signs_of_mental_illness, threat_level)
MI_threat
prop.table(MI_threat, margin = 2)

# MI with flee - not fleeing is far higher than the other three options
MI_flee = table(signs_of_mental_illness, flee)
MI_flee
prop.table(MI_flee, margin = 2)

# MI with body_camera - higher with body camera
MI_body_camera = table(signs_of_mental_illness, body_camera)
MI_body_camera
prop.table(MI_body_camera, margin = 2)

# MI with age - mean age a bit higher among MI than not MI
plot(signs_of_mental_illness, age)

# MI with year - this is interesting, steadily going down YoY, then big drop off
# from 2020 to 2021
MI_year = table(signs_of_mental_illness, year(date))
MI_year
prop.table(MI_year, margin = 2)

# MI with month - proportions relatively constant across months
MI_month = table(signs_of_mental_illness, month(date))
MI_month
prop.table(MI_month, margin = 2)

# MI with day - not much of a pattern, though seemingly lower around end of month
MI_day = table(signs_of_mental_illness, day(date))
MI_day
prop.table(MI_day, margin = 2)

# MI with day of week - not much of a pattern
MI_day_of_week = table(signs_of_mental_illness, wday(date, label=TRUE))
MI_day_of_week
prop.table(MI_day_of_week, margin = 2)

# MI with armed - knife and other higher than the rest, vehicle very low
MI_armed = table(signs_of_mental_illness, armed)
MI_armed
prop.table(MI_armed, margin = 2)

MI_year_race = data %>% group_by(year(date), race, signs_of_mental_illness) %>% count()

