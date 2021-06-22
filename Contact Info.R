#####
# Contact Info.R
# This document provides the code to complete the following tasks:
# 1. Read in necessary datasets (IMG metadata and GEM data)
# 2. Join those tables into one table
# 3. Separate the entries which have contact information, and those which do not
# 4. Summarize which MAGs come from which study centers, and what percentage from each study center have contact information attached
# 5. Create a .csv file which can serve as a mail merge data file. It will have the fields: Contact Name, Contact Email, Projects and Number of Projects. 
#####

#Install and load necessary packages tidyverse and dplyr
install.packages('dplyr', dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages('tidyverse', dependencies=TRUE, repos='http://cran.us.r-project.org')
#library(tidyverse)
library(dplyr)

#Read in the GEM data. It is saved on Marie in the current wd, but the data comes from: 
# https://portal.nersc.gov/GEM/genomes/genome_metadata.tsv 

gem_df <- read.csv("/home/mhibbs1/big_blast_data_analysis/gem_data.csv", na = c("", "NA", "NULL"))

#Read in the IMG metadata file. It is saved on Marie in the current wd, but the data comes from: 
# https://img.jgi.doe.gov/cgi-bin/mer/main.cgi
#To avoid an error, when reading in the file, specify that spaces, "NA" and "NULL" should be marked as na 
img_df <- read.csv("/home/mhibbs1/big_blast_data_analysis/img_new_data.csv", na = c("", "NA","NULL"))
#Use R pipe syntax to rename the column "IMG.Genome.ID" as "metagenome_id". This will allow us to merge later.
img_df <- img_df %>% rename(c("metagenome_id" = "IMG.Genome.ID"))


# Integrate metadata into each genome using a left join by the field "metagenome_id" 
gems_joined <- gem_df %>%
  left_join(img_df, by = c("metagenome_id"))

#Find which entries are missing the field "Contact Name" 
na_vals <- is.na(gems_joined$'Contact.Name')
na_vals2 <- which(grepl("TRUE", na_vals))

#Create a dataframe that includes only the data with contact info
gems_with_contact <- gems_joined[-c(na_vals2), ]    
#Create a dataframe that includes data without contact information 
gems_no_contact <- gems_joined[na_vals, ]

#Summarize sequencing centers Part I: frequency, and percent of total sequencing centers in gem dataset
sc_summary <- as.data.frame(table(gems_joined$`Sequencing.Center`))
sc_summary$Percentage <- prop.table(sc_summary[ , 2])
sc_summary[ , "Percentage"] = sc_summary[ , "Percentage"]*100
sc_summary_sorted <- sc_summary[order(sc_summary$Percentage, decreasing = TRUE),]

#Summarize sequencing centers Part II: amount with contact information for each seq. center, total frequency in gem dataset, 
#perecent of total sequencing centers in gem dataset, percent of each center which has contact information
sc_with_contact <- as.data.frame(table(gems_with_contact$`Sequencing.Center`))
sc_wc_joined <- sc_with_contact %>%
  left_join(sc_summary, by = c("Var1"))  %>%
  rename(c("Sequencing Center" = "Var1", 
           "Total_Frequency" = "Freq.y",
           "Percent_of_Total" = "Percentage",
           "Freq_w_Contact" = "Freq.x"))
sc_wc_joined$Percent_w_Contact <- (sc_wc_joined$Freq_w_Contact/sc_wc_joined$Total_Frequency)*100

#To start making a list of contact information for the mail merge, select only the fields that are relavent, and only select from those which have contact information listed
#This means we take the Contact Name, Email, Submission ID, and Study Name from the table called "gems_with_contact"
cont_df<- as.data.frame(gems_with_contact[ ,c("Contact.Name", "Contact.Email","IMG.Submission.ID", "Study.Name")])
#Get rid of any entries that have the same IMG Submission ID; we treat each UNIQUE Submission ID as a sinlge, unique study
#We can discard entries with duplicate Submission ID's with the following code 
cont_df2 <- cont_df[!duplicated(cont_df$IMG.Submission.ID), ]

#Split our contact info dataframe(cont_df) into a list, using the Contact Name as the factor. This will give us a list where each element of the list includes all
#studies by one single person 
t <- split(cont_list, as.factor(cont_list$Contact.Name))
#Merge the columns "IMG.Submission.ID" with "Study.Name" and separate them by a colon
tl <- lapply(t, function(x) within(x, C <- paste(IMG.Submission.ID, Study.Name, sep=':')))
#Put all Submission ID's and Study Names into a single cell that will correlate with the PI's name; then save those cells separately and turn into a dataframe
tl2 <- lapply(tl, function(x) aggregate(C~Contact.Name, x, paste, collapse="\n"))
tl3 <- lapply(tl2, function(x) x[ , "C"])
proj_info <- data.frame(matrix(unlist(tl3), nrow=length(tl3), byrow=T))

#Need to order the contact information dataframe alphabetically by Contact Name, 
#because this is the order the proj_info df is in because of how the factors were organized in the split 
cont_df2 <- cont_df2[order(cont_df2$Contact.Name),] 
n_r <- lapply(tl, nrow)
n_r_df <- data.frame(matrix(unlist(n_r), nrow=length(n_r), byrow=T))
#bind the contact information dataframe (cont_df2) with the project information, and rename the last column something meaningful
cont_list3 <- cbind(cont_list2, proj_info, n_r_df)
colnames(cont_list3) <- c("contact_name", "contact_email", "proj_id:study_name", "n_studies")

#Finally, we can write the file to the directory: 
#write.csv(cont_list3, file = "permission_email_information.csv")

