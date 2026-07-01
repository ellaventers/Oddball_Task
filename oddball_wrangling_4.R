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

ggplot(combined_data, aes(x = RT)) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 30) + facet_wrap(~session)
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
#There is a significant difference between the scaled RT value for the novel sound between the baseline and followup sessions with the followup session having a higher reaction time with a p value less than 0.05, this was not expected and could be due to smaller followup sample size.

#t-test for score between sessions for novel sound
t.test(combined_data$score[combined_data$session==0&combined_data$snd=="novel"],combined_data$score[combined_data$session==1&combined_data$snd=="novel"], var.equal=FALSE)
#t = -3.4778, df = 405.32, p-value = 0.0005603
#mean of x mean of y 
#0.1882353 0.3083333 
#There is a significant difference in score between the baseline and followup for the novel sound with the followup session having higher scores and a p-value less than 0.05 and this was expected.
