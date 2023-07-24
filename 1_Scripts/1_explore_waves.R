#### SETUP ####
here::i_am("1_Scripts/0_data_cleaning_hsa.R")
source("global_options.R")

#### LOAD DATA ####
df = read.csv(here("0_Data", "Output", "results_weighted.csv"))

ggplot(df %>% filter(is_smoothed==0), aes(x = spec_deaths_1, y = sens_deaths_1)) + geom_point() +
  facet_grid(shapelet_size~trigger_lookback)

# We could:
# Classify everything as a 0.
# Or everything as 1.
# We probably don't want to do either of these.

# Sensitivity:
# What is the probability: classified as 1 | true 1

# Specificity:
# What is the probability: classified as 0 | true 0

# Classes
# 1) bad outcome happening w/i next 4 weeks (but not yet happening)
# 2) bad outcome currently happening
# 3) bad outcome not happening and not happening w/i next 4 weeks

# Hold out:
# 20% oF HSAs
# last few months of data

# Fit shaplets on training data
# Set up to look at performance in training & test data