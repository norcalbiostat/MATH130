library(googlesheets)
library(dplyr)

# Open up google and ask for permissions
gs_auth() 

# Get the sheet info for the spreadsheet you want
sheet_info <- gs_title("130 EDA Peer Grading Rubric (Responses)")

# read the data into a data frame
pr_raw <- sheet_info %>% gs_read()

# rename to usable names
names(pr_raw) <- c("timestamp", "author", "repro", "desc", "univ1", "univ2", "bivar", "summary", "org", "email", "blank", "upload")

# drop all but this semester
pr_raw$date <- as.Date(strptime(pr_raw$timestamp, format="%m/%d/%Y"))
pr <- pr_raw %>% filter(as.Date(date) > "2019-03-02" & as.Date(date) < "2019-06-01") 

# Number of reviews per person -- for grading peer reviews
table(pr$email)

# Identify instructor review vs peer review
pr$IR <- ifelse(grepl("rdonatello", pr$email), "Instructor Review", "Peer Review")
pr$IR[is.na(pr$IR)] <- "Peer Review"

# average score per review
pr <- pr %>% mutate(score = rowMeans(pr[,c('repro', 'desc', 'univ1', 'univ2', 'bivar', 'summary', 'org')], 
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



# write out
write.table(table(pr$email), "nreviews_per_person.txt", sep="\t")
grades_for_ed <- fg %>% select(author, `Peer Review`)
write.table(grades_for_ed, "final_grades.txt", sep="\t")

# Compile numeric reviews to return to author as a zip file
setwd("C:/Box/Teaching/130/08 S19")
ppl <- unique(pr$author)
ppl.names <- paste0("EDA_", gsub(" ", "" ,ppl))
all.files <- list.files(path="EDA/", full.names = TRUE)

for (p in 1:length(ppl)){
  filename <- paste0(ppl.names[p], "_scores.csv")
  person <- pr %>% filter(author==ppl[p]) %>% 
                   select(IR, repro, desc, univ1, bivar, org, score) %>%
                   write.csv(filename, row.names = FALSE)
  zip(paste0(ppl.names[p], ".zip"), files = c(
    all.files[grep(ppl.names[p], all.files, ignore.case=TRUE)], #not working
    filename))
  file.remove(filename)
}




