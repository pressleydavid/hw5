library(tidyverse)
library(waldo)
library(naniar)
library(ggplot2)


d1=read.table("student-mat.csv",sep=";",header=TRUE)
str(d1)
class(d1)
glimpse(d1)

d2=read.table("student-por.csv",sep=";",header=TRUE)
str(d2)
class(d2)
glimpse(d2)


d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

d3_tibbs <- as_tibble(d3)
count(d3_tibbs)

## Import as tibble (tidyverse)
math_tibbs <- as_tibble(d1)
math_na <- any(is.na(math_tibbs))
math_missing <- math_tibbs |>
  filter(if_any(everything(), ~ . == ""))

port_tibbs <- as_tibble(d2)
port_na <- any(is.na(port_tibbs))
port_missing <- port_tibbs |>
  filter(if_any(everything(), ~ . == ""))
combined <- math_tibbs |>
  inner_join(port_tibbs, by = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
count(combined)


### Issue on inner_join of raw data
# Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 79 of `x` matches multiple rows in `y`.
# ℹ Row 79 of `y` matches multiple rows in `x`.
# ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
port_row_79 <- port_tibbs |> slice(79)
math_row_79 <- math_tibbs |> slice(79)
combo_row_79 <- bind_rows(port_row_79, math_row_79)

#assume the merge is sufficient with the number of obs between both and include the # , relationship = "many-to-many" option
#document the findings
combined <- math_tibbs |>
  inner_join(port_tibbs, by = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")), relationship = "many-to-many"
count(combined)

### Determine if there are duplicate rows
# count(distinct(math_dupes))
# distinct(port_dupes)
#
#
# # Compare that all values are equal between merge and inner_join
# diffs <- waldo::compare(port_row_79, math_row_79)
# print(diffs)
#
# all_equal <- all.equal(combined,d3_tibbs)
# print(all_equal)
#
# diff_address <- setdiff(combined$address, d3_tibbs$address)
# print(diff_address)


combined <- math_tibbs |>
  inner_join(port_tibbs, by = c("school","sex","age","address","famsize",
                                "Pstatus","Medu","Fedu","Mjob","Fjob","reason",
                                "guardian","traveltime","studytime","failures",
                                "schoolsup","famsup","activities","nursery",
                                "internet", "romantic","famrel","freetime",
                                "goout","Dalc","Walc","health"),
             suffix = c(".math", ".port")
  )
# unique(combined$guardian)
# Using factor() to relabel the levels
combined <- combined |>
  mutate(school = factor(school,
                         levels = c("GP", "MS"),
                         labels = c("Gabriel Pereira", "Mousinho da Silveira")),
         address = factor(address,
                         levels = c("U", "R"),
                         labels = c("Urban", "Rural")),
         reason = factor(reason,
                         levels = c("course", "home", "reputation", "other"),
                         labels = c("course", "home", "reputation", "other")),
         guardian = factor(guardian,
                           levels = c("mother", "father", "other"),
                           labels = c("mother", "father", "other")),
         Mjob = factor(Mjob,
                       levels = c("at_home","health","other","services","teacher"),
                       labels = c("at_home","health","other","services","teacher")),
         Fjob = factor(Fjob,
                       levels = c("teacher","other","services","health","at_home"),
                       labels = c("teacher","other","services","health","at_home"))

  )
#check levels are applied according to underlying data
#"look at how the data are stored and see if everything makes sense"
combined$school
levels(combined$school)
combined$address
levels(combined$address)
combined$reason
levels(combined$reason)
combined$guardian
levels(combined$guardian)
combined$Mjob
levels(combined$Mjob)
combined$Fjob
levels(combined$Fjob)

#document missing values in data

# Find the positions of NA values in the tibble
na_positions <- which(is.na(combined), arr.ind = TRUE)

# Print the positions of the NA values (row and column indices)
print(na_positions)

print(combined_na)
combined_missing <- combined |>
  filter(if_any(everything(), ~ . == ""))

# Visualize missing data in the tibble
vis_miss(combined)

#1-way table (school)
school_table <- table(combined$school)
print(school_table)

#2-way table (school by address)
school_by_address_table <- table(combined$school,combined$address)
print(school_by_address_table)

#3-way table (school by address by reason)
school_x_address_by_reason <- table(combined$address,combined$school,combined$reason)
print(school_x_address_by_reason)

#Conditional 2-way table
#filter first
subset_sch_GP <- combined |>
  filter(school == "Gabriel Pereira")

address_x_reason <- table(subset_sch_GP$address, subset_sch_GP$reason)
print(address_x_reason)

#subset a 3-way table
subset_3_way <- school_x_address_by_reason[,"Gabriel Pereira",]
print(subset_3_way)

#Create 2-way with group_by() and summarize()
group_by_2_way <- combined |>
  filter(school == "Gabriel Pereira") |>
  group_by(school, address, reason) |>
  summarize(count = n(), .groups = "drop")

print(group_by_2_way)

group_by_2_way_wide <- group_by_2_way |>
  pivot_wider(names_from = address, values_from = count, values_fill = 0)
print(group_by_2_way_wide)


##create plots
### Create a stacked bar graph with ggplot2
ggplot(combined, aes(x = address, fill = reason)) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Reasons by Address",
       x = "Address (Urban/Rural)",
       y = "Count",
       fill = "Reason") +
  theme_minimal()

### Create a side-by-side bar graph with ggplot2
ggplot(combined, aes(x = address, fill = reason)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Reasons by Address (Side-by-Side)",
       x = "Address (Urban/Rural)",
       y = "Count",
       fill = "Reason") +
  theme_minimal()


#summarize by center and spread (G3, age, absence)
summary_stats <- combined |>
  summarize(
    g3_mean = mean(G3.math, na.rm = TRUE),
    g3_median = median(G3.math, na.rm = TRUE),
    g3_sd = sd(G3.math, na.rm = TRUE),
    g3_iqr = IQR(G3.math, na.rm = TRUE),

    age_mean = mean(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_iqr = IQR(age, na.rm = TRUE),

    absences_mean = mean(absences.math, na.rm = TRUE),
    absences_median = median(absences.math, na.rm = TRUE),
    absences_sd = sd(absences.math, na.rm = TRUE),
    absences_iqr = IQR(absences.math, na.rm = TRUE),
  )
print(summary_stats)

#meaningful subset
summary_stats_grp_by <- combined |>
  filter(school == "Gabriel Pereira") |>
  group_by(school, address, reason) |>
  summarize(
    g3_mean = mean(G3.math, na.rm = TRUE),
    g3_median = median(G3.math, na.rm = TRUE),
    g3_sd = sd(G3.math, na.rm = TRUE),
    g3_iqr = IQR(G3.math, na.rm = TRUE),

    age_mean = mean(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_iqr = IQR(age, na.rm = TRUE),

    absences_mean = mean(absences.math, na.rm = TRUE),
    absences_median = median(absences.math, na.rm = TRUE),
    absences_sd = sd(absences.math, na.rm = TRUE),
    absences_iqr = IQR(absences.math, na.rm = TRUE),
    .groups = "drop"
  )
print(summary_stats_grp_by)


summary_stats_single_grp <- combined |>
  group_by(school) |>
  summarize(
    g3_mean = mean(G3.math, na.rm = TRUE),
    g3_median = median(G3.math, na.rm = TRUE),
    g3_sd = sd(G3.math, na.rm = TRUE),
    g3_iqr = IQR(G3.math, na.rm = TRUE),

    age_mean = mean(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_iqr = IQR(age, na.rm = TRUE),

    absences_mean = mean(absences.math, na.rm = TRUE),
    absences_median = median(absences.math, na.rm = TRUE),
    absences_sd = sd(absences.math, na.rm = TRUE),
    absences_iqr = IQR(absences.math, na.rm = TRUE),
  )
print(summary_stats_single_grp)

summary_stats_double_grp <- combined |>
  group_by(school, address) |>
  summarize(
    g3_mean = mean(G3.math, na.rm = TRUE),
    g3_median = median(G3.math, na.rm = TRUE),
    g3_sd = sd(G3.math, na.rm = TRUE),
    g3_iqr = IQR(G3.math, na.rm = TRUE),

    age_mean = mean(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_iqr = IQR(age, na.rm = TRUE),

    absences_mean = mean(absences.math, na.rm = TRUE),
    absences_median = median(absences.math, na.rm = TRUE),
    absences_sd = sd(absences.math, na.rm = TRUE),
    absences_iqr = IQR(absences.math, na.rm = TRUE),
  )
print(summary_stats_double_grp)

#Correlation matrix for numerics
num_vars <- combined |>
  select(where(is.numeric))

corr_matrix <- cor(num_vars, use = "complete.obs")
print(corr_matrix)

#Historgram of G3 across school
# Create histogram for G3 across school
ggplot(combined, aes(x = G3.math, fill = school)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +  # position = "identity" overlays the bars
  labs(title = "Distribution of Math Grades (G3.math) by School",
       x = "Math Grade (G3.math)",
       y = "Count",
       fill = "School") +
  theme_minimal()

#kernel density of absences by school
ggplot(combined, aes(x = absences.math, fill = school)) +
  geom_density(alpha = 0.6) +
  labs(title = "Kernel Density of Absences by School",
       x = "Number of Absences",
       y = "Density",
       fill = "School") +
  theme_minimal()

# Boxplot for G3 across school
ggplot(combined, aes(x = school, y = G3, fill = school)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot of Final Grades (G3) by School",
       x = "School",
       y = "Final Grade (G3)",
       fill = "School") +
  theme_minimal()

library(ggplot2)

# Scatterplot of G3 vs. absences with jitter and color by school
ggplot(combined, aes(x = absences.math, y = G3.math, color = school)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.8) +  # Jitter to avoid overlap
  labs(title = "Final Grade (G3) vs. Absences by School",
       x = "Number of Absences",
       y = "Final Grade (G3)",
       color = "School") +
  theme_minimal()

# Scatterplot of G3 vs. age with jitter and color by sex
ggplot(combined, aes(x = age, y = G3.math, color = sex)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.8) +  # Jitter to avoid overlap
  labs(title = "Final Grade (G3) vs. Age by Sex",
       x = "Age",
       y = "Final Grade (G3)",
       color = "Sex") +
  theme_minimal()

library(ggplot2)

# Scatterplot of G3 vs. absences with faceting by sex and color by school
ggplot(combined, aes(x = absences.math, y = G3.math, color = school)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.8) +
  labs(title = "Final Grade (G3) vs. Absences, Faceted by Sex",
       x = "Number of Absences",
       y = "Final Grade (G3)",
       color = "School") +
  theme_minimal() +
  facet_wrap(~ sex) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.spacing = unit(1, "lines")
  )


# Scatterplot of G3 vs. age with faceting by school and color by sex
ggplot(combined, aes(x = age, y = G3.math, color = sex)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.8) +  # Jitter to avoid overlap
  labs(title = "Final Grade (G3) vs. Age, Faceted by School",
       x = "Age",
       y = "Final Grade (G3)",
       color = "Sex") +
  theme_minimal() +
  facet_wrap(~ school) +  # Facet by the 'school' variable
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Add a black border around each facet
    panel.spacing = unit(1, "lines")  # Increase the space between facets
  )

library(ggplot2)

# Scatterplot of G3 vs. age, faceted by school and sex
ggplot(combined, aes(x = age, y = G3.math, color = school)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.8) +  # Jitter to avoid overlap
  labs(title = "Final Grade (G3) vs. Age, Faceted by School and Sex",
       x = "Age",
       y = "Final Grade (G3)",
       color = "School") +
  theme_minimal() +
  facet_grid(sex ~ school) +  # Facet by 'sex' for rows and 'school' for columns
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add a border around each facet
    panel.spacing = unit(1, "lines")  # Increase the space between facets
  )



