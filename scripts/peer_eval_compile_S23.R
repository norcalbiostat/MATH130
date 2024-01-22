library(googlesheets4)  # https://googlesheets4.tidyverse.org/
library(dplyr)

# New users may have to authorize googlesheets4 to access the GDrive API first. 

# read the data into a data frame
pr_raw <- read_sheet("https://docs.google.com/spreadsheets/d/17VhpLa9AZOkeOUYtxyog5NwXL389HLKP7v8VqVmDdSU/edit#gid=1340211106")

# rename to usable names
names(pr_raw) <- c("timestamp", "author", "repro", "desc", "univ", "univ2", "bivar", "summary", "org", "email", "blank", "upload")

# drop all but this semester (update timestamps where needed)
pr <- pr_raw %>% filter(as.Date(timestamp) > "2022-08-01" & as.Date(timestamp) < "2022-12-01") 


# Identify instructor review vs peer review
pr$IR <- ifelse(grepl("rdonatello|nlytal|eroualdes|klgray", pr$email), "Instructor Review", "Peer Review")
pr$IR[is.na(pr$IR)] <- "Peer Review"

# average score per review
pr <- pr %>% mutate(score = rowMeans(pr[,c('repro', 'desc', 'univ', 'bivar', 'org')], 
                                     na.rm=TRUE), 
                    author = tolower(author))

# Number of reviews per author
table(pr$author, pr$IR) 

# group per reviewer, separate by IR/PR, calculate weighted average, multiply by total pts for report. 
fg <- pr %>% group_by(author, IR) %>%
        summarise(pr.ave = mean(score)) %>%
        tidyr::spread(IR, pr.ave) %>% ungroup() %>%
        mutate(wtd.grade = round((.7*`Instructor Review` + .3*`Peer Review`)/10*20,0), 
               wtd.grade = ifelse(is.na(wtd.grade), round(`Instructor Review`/10*20,0), wtd.grade)) %>%
        print(n=Inf)


#######################################
#       THINGS TO GRADE
#######################################

# Number of reviews per person -- give credit for reviewing peer work
table(pr$email)

# Full set of grades: Instructor, Peer & weighted grade
fg


