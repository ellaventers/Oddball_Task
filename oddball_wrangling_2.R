#EllaVenters
#6/3/26
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
