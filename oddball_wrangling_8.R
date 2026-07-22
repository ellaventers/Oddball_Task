#Ella Venters
#7/15/26
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
  mutate(scaledRT=scale(DescTools::Winsorize(RT,val=quantile(RT,c(0.05,0.95),na.rm=TRUE))))
combined_data %>%
  summarize(
    p_value = shapiro.test(na.omit(scaledRT))$p.value)
View(combined_data)

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

#practice or fatigue effects?
combined_data$trial_c <- combined_data$trial - mean(combined_data$trial)
model5 <- lmer(
  scaledRT ~ session * trial_c + (trial_c | ID),
  data = combined_data
)
summary(model5)
#gives warning

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

#make std reference
combined_data$snd <- relevel(as.factor(combined_data$snd), ref="std")
combined_data$snd <- factor(
  combined_data$snd,
  levels = c("std", "tgt", "novel")
)
levels(combined_data$snd)

#model 3 (interaction with random effect)
model3 <- lmer(score ~ session * snd +(1 | ID),
  data = combined_data)
summary(model3)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)                  0.56479    0.07082   17.18732   7.975 3.53e-07
# sessionFollow-up             0.18154    0.01474 4992.54533  12.319  < 2e-16
# sndtgt                       0.01092    0.01686 4977.01780   0.648   0.5172
# sndnovel                    -0.38319    0.01686 4977.01780 -22.722  < 2e-16
# sessionFollow-up:sndtgt     -0.05527    0.02981 4977.01780  -1.854   0.0638
# sessionFollow-up:sndnovel   -0.12365    0.02981 4977.01780  -4.148 3.41e-05

#follow up has significantly higher accuracy than baseline
#novel sound has significantly lower accuracy than standard
#accuracy of novel sound changes at a slower rate from baseline to follow up than standard sound

#emmeans for model 3
posthoc2 <- emmeans(model3,~session|snd)
summary(posthoc2)
contrast(posthoc2,"revpairwise")
# snd = std:
#   contrast               estimate     SE  df z.ratio p.value
# (Follow-up) - Baseline   0.1815 0.0147 Inf  12.319 <0.0001
# snd = tgt:
#   contrast               estimate     SE  df z.ratio p.value
# (Follow-up) - Baseline   0.1263 0.0281 Inf   4.486 <0.0001
# snd = novel:
#   contrast               estimate     SE  df z.ratio p.value
# (Follow-up) - Baseline   0.0579 0.0281 Inf   2.057  0.0397

#for all sounds, follow up has significantly greater accuracy than baseline

#plot posthoc2
posthoc2dataframe<- data.frame(posthoc2)
View(posthoc2dataframe)
pd <- position_dodge(width = 0.5)
ggplot(
  subset(posthoc2dataframe, !is.na(snd)),
  aes(x =snd, y = emmean, ymin=asymp.LCL, ymax=asymp.UCL, color = session, group = session)
) +
  geom_errorbar(position=pd) +
  geom_point(position=pd) +
  labs(
    title = "Mean Accuracy by Sound Type and Session",
    x = "Sound Type",
    y = "Mean Proportion Correct"
  ) +
  theme_classic()

#posthoc3
posthoc3 <- emmeans(model3,~snd|session)
contrast(posthoc3, "revpairwise")
# session = Baseline:
#   contrast    estimate     SE  df z.ratio p.value
# tgt - std     0.0109 0.0169 Inf   0.648  0.7936
# novel - std  -0.3832 0.0169 Inf -22.722 <0.0001
# novel - tgt  -0.3941 0.0216 Inf -18.210 <0.0001
# 
# session = Follow-up:
#   contrast    estimate     SE  df z.ratio p.value
# tgt - std    -0.0443 0.0246 Inf  -1.804  0.1683
# novel - std  -0.5068 0.0246 Inf -20.617 <0.0001
# novel - tgt  -0.4625 0.0316 Inf -14.659 <0.0001

#for both baseline and follow up, novel was significantly less accurate than both standard and target which weren't different from each other

install.packages("lmerTest")
library(lmerTest)

#model 4
model4 <- lmer(data=combined_data,scaledRT~session*snd + score +(1|ID) )
summary(model4)
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)                 -0.03824    0.10541   20.39605  -0.363  0.72048
# sessionFollow-up             0.12183    0.04046 4090.39665   3.011  0.00262
# sndtgt                       0.28870    0.04637 4115.74927   6.227 5.24e-10
# sndnovel                     0.65187    0.08251 4125.28739   7.900 3.54e-15
# score                        0.00349    0.04764 3680.71824   0.073  0.94160
# sessionFollow-up:sndtgt     -0.14006    0.07966 4115.28212  -1.758  0.07877
# sessionFollow-up:sndnovel    0.56301    0.12835 4124.31626   4.386 1.18e-05  

#follow up has a significantly longer reaction time than baseline
#target has a significantly longer reaction time than standard
#novel has a significantly longer reaction time than standard
#reaction time to novel sound changes at a faster rate from baseline to follow up than standard sound

#emmeans for model 4
posthoc4 <- emmeans(model4, ~session|snd)
contrast(posthoc4, "revpairwise")
# snd = std:
#   contrast               estimate     SE  df z.ratio p.value
# (Follow-up) - Baseline   0.1218 0.0405 Inf   3.011  0.0026
# snd = tgt:
#   contrast               estimate     SE  df z.ratio p.value
# (Follow-up) - Baseline  -0.0182 0.0757 Inf  -0.241  0.8096
# snd = novel:
#   contrast               estimate     SE  df z.ratio p.value
# (Follow-up) - Baseline   0.6848 0.1250 Inf   5.491 <0.0001

#there is a significant increase in reaction time to standard sound between baseline and follow up
#there is a significant increase in reaction time to novel sound between baseline and follow up

#plot posthoc4
posthoc4dataframe<- data.frame(posthoc4)
View(posthoc4dataframe)
pd <- position_dodge(width = 0.5)
ggplot(
  subset(posthoc4dataframe, !is.na(snd)),
  aes(x =snd, y = emmean, ymin=asymp.LCL, ymax=asymp.UCL, color = session, group = session)
) +
  geom_errorbar(position=pd) +
  geom_point(position=pd) +
  labs(
    title = "Mean Scaled Reaction Time by Sound Type and Session",
    x = "Sound Type",
    y = "Mean Scaled Reaction Time"
  ) +
  theme_classic()

#posthoc5
posthoc5 <- emmeans(model4, ~snd|session)
contrast(posthoc5, "revpairwise")
# session = Baseline:
#   contrast    estimate     SE  df z.ratio p.value
# tgt - std      0.289 0.0464 Inf   6.227 <0.0001
# novel - std    0.652 0.0825 Inf   7.900 <0.0001
# novel - tgt    0.363 0.0904 Inf   4.017  0.0002
# 
# session = Follow-up:
#   contrast    estimate     SE  df z.ratio p.value
# tgt - std      0.149 0.0648 Inf   2.294  0.0566
# novel - std    1.215 0.0987 Inf  12.312 <0.0001
# novel - tgt    1.066 0.1110 Inf   9.565 <0.0001

#for baseline, all sounds have significantly different reaction times (std, tgt, novel)
#for follow up, novel has significantly longer reaction time than target and standard, which are not significantly different

#graphing model 4 results (old line graph, ignore)
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

#graphing model 3 results (old line graph, ignore)
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

#start adding RedCap demographic data
demoData <- read.csv("demo_oddball_data.csv")
View(demoData)
library(dplyr)
demoData <- demoData %>%
  rename(
    ID = record_id,
    session = redcap_event_name
  )
demoData<- demoData %>%
  mutate(
    session = recode(session,
    "baseline_arm_1" = "Baseline",
    "week_12__follow_up_arm_1" = "Follow-up")
  )
demoData <- demoData %>%
  mutate(ID = str_extract(ID, "\\d+"))

str(combined_data$ID)
str(demoData$ID)
#ID chr to int in demo
str(combined_data$session)
str(demoData$session)
#session chr to factor in demo
demoData <- demoData %>%
  mutate(
    ID = as.integer(ID),
    session = as.factor(session)
  )
task_demoData <- combined_data %>%
  inner_join(demoData, by = c("ID", "session"))
View(task_demoData)
nrow(combined_data)
nrow(task_demoData)
nrow(demoData)
anti_join(
  combined_data,
  demoData,
  by = c("ID", "session")
) %>%
  distinct(ID, session)
task_demoData <- combined_data %>%
  left_join(demoData, by = c("ID", "session"))

#change sex to factor
task_demoData <- task_demoData %>%
  mutate(
    sex = recode(sex,
                 `1` = "Male",
                 `2` = "Female")
  )
task_demoData <- task_demoData %>%
  mutate(
    sex = factor(sex, levels = c("Male", "Female"))
  )
levels(task_demoData$sex)
str(task_demoData$age)

#fill down age and sex for both sessions
task_demoData <- task_demoData %>%
  arrange(ID, session, trial) %>%
  group_by(ID) %>%
  fill(age, sex, .direction = "downup") %>%
  ungroup()

#calculating percent change in BPRS and positive sx
percent_changeData <- task_demoData %>%
  group_by(ID, session) %>%
  summarise(
    bprs_total = first(bprs_total),
    positive_sx = first(positive_sx),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = session,
    values_from = c(bprs_total, positive_sx)
  ) %>%
  mutate(
    BPRS_percent_change =
      100 * (`bprs_total_Follow-up` - bprs_total_Baseline) /
      bprs_total_Baseline,
    Positive_sx_percent_change =
      100 * (`positive_sx_Follow-up` - positive_sx_Baseline) /
      positive_sx_Baseline
  ) %>%
  select(ID, BPRS_percent_change, Positive_sx_percent_change)
View(percent_changeData)
#NAs are IDs without follow-up data


