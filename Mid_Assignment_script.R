#all the code for Rmarkdown file are in here, but may not in the same order
# import necessary library
library(tidyverse)
library(dplyr)
library(ggridges)
library(skimr)
library(ggpubr)
library(descriptr)


# importing raw data
raw_data <- read.csv("assignment_1_data.csv")

# converted data into long format, meanwhile, change Conditions data into meaningful way, because condition4 seems misplaced.
converted_data <- gather(
  data = raw_data,
  key = "Conditions",
  value = "RT",
  2:5
)

converted_data <- converted_data %>%
  mutate(Conditions = factor(Conditions)) %>%
  mutate(
    Conditions = case_when(
      Conditions == "Condition1" ~ "ExpNum_Num",
      Conditions == "Condition2" ~ "ExpNum_Let",
      Conditions == "Condition3" ~ "ExpLet_Num",
      Conditions == "Condition4" ~ "ExpLet_Let"
    )
  ) %>%
  mutate(
    Expectation = case_when(
      Conditions == "ExpNum_Num" ~ "Number",
      Conditions == "ExpNum_Let" ~ "Number",
      Conditions == "ExpLet_Let" ~ "Letter",
      Conditions == "ExpLet_Num" ~ "Letter"
    )
  ) %>%
  mutate(
    Target = case_when(
      Conditions == "ExpNum_Num" ~ "Number",
      Conditions == "ExpNum_Let" ~ "Letter",
      Conditions == "ExpLet_Let" ~ "Letter",
      Conditions == "ExpLet_Num" ~ "Number"
    )
  ) %>%
  mutate(Expectation = factor(Expectation)) %>%
  mutate(Target = factor(Target))

# reorder factor sequence
converted_data <- converted_data %>%
  mutate(Conditions = factor(Conditions, levels = c("ExpNum_Num", "ExpNum_Let", "ExpLet_Let", "ExpLet_Num")))

# It can also be done in another way.
converted_data2 <- raw_data %>%
  pivot_longer(
    cols = c(Condition1, Condition2, Condition3, Condition4),
    names_to = "Conditions",
    values_to = "RT"
  )

# take a look about data summary.
converted_data %>%
  dplyr::group_by(Conditions) %>%
  skim() %>%
  dplyr::filter(skim_variable == "RT")

# descriptr can also do this
converted_data %>%
  filter(Conditions == "ExpNum_Num") %>%
  ds_summary_stats()
converted_data %>%
  filter(Conditions == "ExpNum_Let") %>%
  ds_summary_stats()
converted_data %>%
  filter(Conditions == "ExpLet_Num") %>%
  ds_summary_stats()
converted_data %>%
  filter(Conditions == "ExpLet_Let") %>%
  ds_summary_stats()

# take a look at data in each condition.
ggplot(data = converted_data, aes(x = Conditions, y = RT, fill = Conditions)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(
    fun.data = "mean_sd",
    geom = "errorbar",
    color = "black",
    width = .25,
    position = position_dodge(.9)
  ) +
  stat_compare_means(
    comparisons = list(c("ExpLet_Let", "ExpLet_Num"), c("ExpNum_Num", "ExpNum_Let")),
    paired = TRUE,
    symnum.args = list(
      cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf),
      symbols = c("****", "***", "**", "*", "ns")
    )
  ) +
  labs(title = "Bar Chart to Compare Different Conditions") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))




# take a look at different conditions
ggplot(data = converted_data, aes(x = RT)) +
  geom_density(aes(fill = Conditions), alpha = .3) +
  facet_wrap(~Conditions) +
  labs(y = "Density") +
  theme(legend.position = "none")

ggplot(data = converted_data, mapping = aes(x = Expectation:Target, y = RT, colour = Conditions, )) +
  geom_violin() +
  geom_jitter(width = .5, alpha = .5) +
  geom_boxplot(alpha = .5) +
  guides(colour = "none") +
  # facet_wrap(~Target) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = .5)) +
  labs(x = "Expection x Target")

ggplot(data = converted_data) +
  geom_density_ridges(mapping = aes(x = RT, y = Conditions, fill = Conditions), alpha = .7) +
  theme(legend.position = "none")

# summary plots
ggplot(mapping = aes(
  x = rank(RT),
  y = RT
)) +
  geom_point(
    data = filter(converted_data, Conditions == "ExpNum_Num"),
    color = "red"
  ) +
  geom_point(
    data = filter(converted_data, Conditions == "ExpNum_Let"),
    color = "green"
  ) +
  geom_point(
    data = filter(converted_data, Conditions == "ExpLet_Let"),
    color = "orange"
  ) +
  geom_point(
    data = filter(converted_data, Conditions == "ExpLet_Num"),
    color = "blue"
  )

# distribution check
ggplot(data = converted_data, aes(sample = RT, colour = factor(Conditions))) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~Conditions)
