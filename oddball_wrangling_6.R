#Ella Venters
#7/1/26
#Oddball Data Wrangling 

#set up
setwd("C:/Users/emv55/OneDrive/Documents/DataForLabs")
library(tidyverse)

#first try to take out numbers
cleanData <- read.csv("clean_data(1).csv")
View(cleanData)
sub1873B <- read.delim("1873_BL.txt")
View(sub1873B)
write.csv(sub1873B, "sub1873B.csv")
sub1873B <- read.csv("sub1873B.csv")
View(sub1873B)
text_lines <- readLines("1873_BL.txt")
numbers <- str_extract_all(text_lines, "\\d+")
df <- data.frame(Numbers = unlist(numbers))
data1 <- write.csv(df, "numbers_only.csv", row.names = FALSE)
View(df)
num_only <-read.csv("numbers_only.csv")
View(num_only)

#extracting and making wide format
text_lines <- readLines("1873_BL.txt")
df <- tibble(text = text_lines)
df_wide <- df %>%
  extract(
    text,
    into = c("trial", "pushed", "score", "RT", "sndon", "snd"),
    regex = "trial\\s+(\\d+)\\s+pushed\\s+(\\d+)\\s+score\\s+(\\d+)\\s+RT\\s+(-Inf|[0-9.]+)\\s+sndon\\s+([0-9.]+)\\s+snd\\s+(\\S+)"
  ) %>%
  drop_na(trial) %>%
  mutate(
    trial = as.integer(trial),
    pushed = as.integer(pushed),
    score = as.integer(score),
    RT = as.numeric(RT),
    sndon = as.numeric(sndon)
  )
View(df_wide)

#adding ID and session
df_wide <- df_wide %>%
  mutate(
    ID = "1873",
    session = 0
  ) %>%
  relocate(ID, session)
View(df_wide)

#create csv
write.csv(df_wide, "1873_BL.csv", row.names = FALSE)
sub1873_1 <- read.csv("1873_BL.csv")
View(sub1873_1)

#function to process files
process_file <- function(file) {
  
  fname <- tools::file_path_sans_ext(basename(file))
  parts <- str_split(fname, "_", simplify = TRUE)
  
  ID <- parts[1]
  session <- ifelse(parts[2] == "BL", 0, 1)
  
  text_lines <- readLines(file)
  
  tibble(text = text_lines) %>%
    extract(
      text,
      into = c("trial", "pushed", "score", "RT", "sndon", "snd"),
      regex = "trial\\s+(\\d+)\\s+pushed\\s+(\\d+)\\s+score\\s+(\\d+)\\s+RT\\s+(-Inf|[0-9.]+)\\s+sndon\\s+([0-9.]+)\\s+snd\\s+(\\S+)"
    ) %>%
    drop_na(trial) %>%
    mutate(
      ID = as.integer(ID),
      session = session,
      trial = as.integer(trial),
      pushed = as.integer(pushed),
      score = as.integer(score),
      RT = as.numeric(RT),
      sndon = as.numeric(sndon)
    ) %>%
    relocate(ID, session)
}

#testing function
newdf <- process_file("1873_FU.txt")
View(newdf)
write.csv(df_wide, "1873_BL.csv", row.names = FALSE)
sub1873_1 <- read.csv("1873_BL.csv")
View(sub1873_1)

#processing all files and combining to csv
files <- c(
  "1873_BL.txt",
  "1873_FU.txt",
  "1942_BL.txt",
  "2207_BL.txt",
  "2207_FU.txt",
  "2220_BL.txt",
  "2236_BL.txt",
  "2236_FU.txt",
  "2261_BL.txt",
  "2321_BL.txt",
  "2344_FU.txt",
  "2362_BL.txt",
  "2363_BL.txt",
  "2363_FU.txt",
  "2378_BL.txt",
  "2378_FU.txt",
  "2381_BL.txt",
  "2383_BL.txt",
  "2393_BL.txt",
  "2395_BL.txt",
  "2464_BL.txt",
  "2475_BL.txt",
  "2475_FU.txt",
  "2489_BL.txt",
  "2489_FU.txt"
)
all_data <- purrr::map_dfr(files, process_file)
write.csv(all_data, "all_subjects.csv", row.names = FALSE)
combined_data <- read.csv("all_subjects.csv")
View(combined_data)

#editing and viewing combined data
class(combined_data$RT)
table(combined_data$ID)
length(unique(combined_data$ID))
table(combined_data$ID,combined_data$session)
table(combined_data$ID,combined_data$session,combined_data$snd) 
library(ggplot2)
ggplot(data=combined_data, aes(x=RT)) + geom_histogram(bins=30)
ggplot(data=combined_data, aes(x=RT)) + geom_histogram(bins=30) + facet_wrap(~ID)


#adding NAs for missing IDs and "-inf" in RT
library(tidyverse)
combined_data <- combined_data %>%
  complete(
    ID,
    session = c(0, 1),
    trial = 1:200)
View(combined_data)
combined_data <- combined_data %>%
  mutate(RT = na_if(RT, -Inf))
View(combined_data)

#remove numbers from each novel sound
library(dplyr)
library(stringr)
combined_data <- combined_data %>%
  mutate(
    snd = str_replace(snd, "^novel\\d+$", "novel")
  )
View(combined_data)

#fixing zeros on graph/changing below 0.10 RT to NA
summary(combined_data$RT)
combined_data <- combined_data %>%
  mutate(
    RT = if_else(RT < 0.10, NA_real_, RT)
  )
View(combined_data)
ggplot(data=combined_data, aes(x=RT)) + geom_histogram(bins=30)

#compare RT means between sessions
ggplot(data=combined_data, aes(x=RT)) + geom_histogram(bins=30) + facet_wrap(~session)
ggplot(combined_data, aes(x = RT)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30) + facet_wrap(~session)
#create new column for density
combined_data

combined_data$session <- factor(
  combined_data$session,
  levels = c(0, 1),
  labels = c("Baseline", "Follow-up")
)
ggplot(combined_data, aes(x = RT)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 30) +
  facet_wrap(~session) +
  labs(
    title = "Distribution of Reaction Times by Session",
    x = "Reaction Time (s)",
    y = "Proportion of Observations"
  )

combined_data %>%
  group_by(session) %>%
  summarize(
    n = sum(!is.na(RT)),
    mean_RT = mean(RT, na.rm = TRUE)
  )
#  session     n mean_RT
#<dbl> <int>   <dbl>
#1       0  2711   0.653
#2       1  1428   0.629
combined_data %>%
  group_by(session, snd) %>%
  summarize(
    n = sum(!is.na(RT)),
    mean_RT = mean(RT, na.rm = TRUE),
    .groups = "drop"
  )
#session snd       n mean_RT
#<dbl> <chr> <int>   <dbl>
#1       0 novel   134   0.783
#2       0 std    2116   0.632
#3       0 tgt     461   0.710
#5       1 novel    94   0.906
#6       1 std    1099   0.603
#7       1 tgt     235   0.640

#testing normality
library(dplyr)
combined_data %>%
  group_by(session) %>%
  summarize(
    p_value = shapiro.test(na.omit())$p.value
  )

#Winsorize data and adding scaled RT variable 
install.packages("DescTools")
library(DescTools)
combined_data <- combined_data %>%
  group_by(session) %>%
  mutate(scaledRT=scale(DescTools::Winsorize(RT,val=quantile(RT,c(0.05,0.95),na.rm=TRUE)))) %>%
  ungroup()
combined_data %>%
  group_by(session) %>%
  summarize(
    p_value = shapiro.test(na.omit(scaledRT))$p.value)

#t-test for scaledRT between sessions for novel sound
t.test(combined_data$scaledRT[combined_data$session==0&combined_data$snd=="novel"],combined_data$scaledRT[combined_data$session==1&combined_data$snd=="novel"], var.equal=FALSE)
#t = -3.5579, df = 197.31, p-value = 0.0004683
#mean of x mean of y 
#0.4820716 1.0334916

#t-test for score between sessions for novel sound
t.test(combined_data$score[combined_data$session==0&combined_data$snd=="novel"],combined_data$score[combined_data$session==1&combined_data$snd=="novel"], var.equal=FALSE)
#t = -3.4778, df = 405.32, p-value = 0.0005603
#mean of x mean of y 
#0.1882353 0.3083333 

#models for accuracy and sound (model 1 and 2)
model1 <- glm(score ~ session + snd,
             data = combined_data,
             family = binomial)
summary(model1)

model2 <- glmer(
  score ~ session + snd +(1 | ID),
  data = combined_data,
  family = binomial
)
summary(model2)
#Fixed effects:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -2.6176     0.5679  -4.609 4.04e-06 ***
#session       1.1396     0.3143   3.625 0.000288 ***
anova(model1,model2, test="Chisq")

combined_data$snd
class(combined_data$session)
library(emmeans)

#model 3 (interaction with random effect)
model3 <- glmer(score ~ session * snd +(1 | ID),
  data = combined_data,
  family = binomial)
summary(model3)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        0.11788    0.62690   0.188  0.85085    
#session1           3.16320    0.24341  12.995  < 2e-16 ***
#sndnovel          -2.94716    0.16548 -17.809  < 2e-16 ***
#sndtgt             0.08171    0.13339   0.613  0.54016    
#session1:sndnovel -2.69047    0.33616  -8.004 1.21e-15 ***
#session1:sndtgt   -0.84645    0.31617  -2.677  0.00742 ** 

#emmeans for model 3
posthoc2 <- emmeans(model3,~session|snd)
summary(posthoc2)
contrast(posthoc2,"revpairwise")
#snd = std:
#  contrast            estimate    SE  df z.ratio p.value
#session1 - session0    3.163 0.243 Inf  12.995 <0.0001
#snd = novel:
#  contrast            estimate    SE  df z.ratio p.value
#session1 - session0    0.473 0.240 Inf   1.967  0.0491
#snd = tgt:
#  contrast            estimate    SE  df z.ratio p.value
#session1 - session0    2.317 0.330 Inf   7.012 <0.0001
posthoc3 <- emmeans(model3,~snd|session)
contrast(posthoc3, "revpairwise")
#session = 0:
#contrast    estimate    SE  df z.ratio p.value
#novel - std  -2.9472 0.165 Inf -17.809 <0.0001
#tgt - std     0.0817 0.133 Inf   0.613  0.8132
#tgt - novel   3.0289 0.198 Inf  15.311 <0.0001

#session = 1:
#contrast    estimate    SE  df z.ratio p.value
#novel - std  -5.6376 0.312 Inf -18.067 <0.0001
#tgt - std    -0.7647 0.287 Inf  -2.668  0.0208
#tgt - novel   4.8729 0.368 Inf  13.236 <0.0001

install.packages("lmerTest")
library(lmerTest)

#model 4
model4 <- lmer(data=combined_data,scaledRT~session*snd + score +(1|ID) )
summary(model4)
#Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)         -0.07049    0.10463   20.46829  -0.674    0.508    
#session1             0.20682    0.04047 4088.07679   5.110 3.37e-07 ***
#sndnovel             0.63527    0.08254 4125.42709   7.696 1.74e-14 ***
#sndtgt               0.27860    0.04638 4115.77247   6.006 2.06e-09 ***
#score                0.00459    0.04765 3664.67444   0.096    0.923    
#session1:sndnovel    0.61760    0.12840 4124.44749   4.810 1.56e-06 ***
#session1:sndtgt     -0.12186    0.07969 4115.29799  -1.529    0.126    

#emmeans for model 4
posthoc4 <- emmeans(model4, ~session|snd)
contrast(posthoc4, "revpairwise")
#snd = std:
#  contrast            estimate     SE  df z.ratio p.value
#session1 - session0    0.207 0.0405 Inf   5.110 <0.0001
#snd = novel:
#  contrast            estimate     SE  df z.ratio p.value
#session1 - session0    0.824 0.1250 Inf   6.608 <0.0001
#snd = tgt:
#  contrast            estimate     SE  df z.ratio p.value
#session1 - session0    0.085 0.0757 Inf   1.123  0.2616

#practice or fatigue effects
combined_data$trial_c <- combined_data$trial - mean(combined_data$trial)
model5 <- lmer(
  scaledRT ~ session * trial_c + (trial_c | ID),
  data = combined_data
)
summary(model5)
#not significant but gives warning

#graphing model 4 results
ggplot(
  subset(combined_data, !is.na(snd)),
  aes(x = factor(snd, levels = c("std", "tgt", "novel")), y = scaledRT, color = session, group = session)
) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  labs(
    title = "Mean Scaled Reaction Time by Sound Type and Session",
    x = "Sound Type",
    y = "Mean Scaled Reaction Time"
  ) +
  theme_classic()

ggplot(
  subset(combined_data, !is.na(snd)),
  aes(x = factor(snd, levels = c("std", "tgt", "novel")), y = RT, color = session, group = session)
) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  labs(
    title = "Mean Reaction Time by Sound Type and Session",
    x = "Sound Type",
    y = "Mean Reaction Time"
  ) +
  theme_classic()

#graphing model 3 results

ggplot(
  subset(combined_data, !is.na(snd)),
  aes(
    x = factor(snd, levels = c("std", "tgt", "novel")),
    y = score,
    color = session,
    group = session
  )
) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent
  ) +
  labs(
    title = "Accuracy by Sound Type and Session",
    x = "Sound Type",
    y = "Proportion Correct",
    color = "Session"
  ) +
  theme_classic()

