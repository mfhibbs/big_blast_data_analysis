read.csv("/srv/data/big_blast/data/deleteme.csv")
install.packages('tidyverse', dependencies=TRUE, repos='http://cran.us.r-project.org')
#library(tidyverse)
library(dplyr)
#https://portal.nersc.gov/GEM/genomes/genome_metadata.tsv more comments on this file etc.
gem_df <- read.csv("gem_data.csv", na = c("", "NA", "NULL"))
#IMG datafile (insert url)
img_df <- read.csv("img_new_data.csv", na = c("", "NA","NULL"))
img_df <- img_df %>% rename(c("metagenome_id" = "IMG.Genome.ID"))
# integrate metadata into each genome
gems_joined <- gem_df %>%
  left_join(img_df, by = c("metagenome_id"))
#Find which entries are missing the field "Contact Name" 
na_vals <- is.na(gems_joined$'Contact.Name')
na_vals2 <- which(grepl("TRUE", na_vals))

#Create a dataframe that includes only the data with contact info
gems_with_contact <- gems_joined[-c(na_vals2), ]    
#Create a dataframe that includes data without contact information 
gems_no_contact <- gems_joined[na_vals, ]

#summarize sequencing center 
sc_summary <- as.data.frame(table(gems_joined$`Sequencing.Center`))
sc_summary$Percentage <- prop.table(sc_summary[ , 2])
sc_summary[ , "Percentage"] = sc_summary[ , "Percentage"]*100
sc_summary_sorted <- sc_summary[order(sc_summary$Percentage, decreasing = TRUE),]

sc_with_contact <- as.data.frame(table(gems_with_contact$`Sequencing.Center`))
sc_wc_joined <- sc_with_contact %>%
  left_join(sc_summary, by = c("Var1"))  %>%
  rename(c("Sequencing Center" = "Var1", 
           "Total_Frequency" = "Freq.y",
           "Percent_of_Total" = "Percentage",
           "Freq_w_Contact" = "Freq.x"))
sc_wc_joined$Percent_w_Contact <- (sc_wc_joined$Freq_w_Contact/sc_wc_joined$Total_Frequency)*100

cont_list <- as.data.frame(gems_with_contact[ ,c("Contact.Name", "Contact.Email","IMG.Submission.ID", "Study.Name")])
cont_list <- cont_list[!duplicated(cont_list$IMG.Submission.ID), ]
t <- split(cont_list, as.factor(cont_list$Contact.Name))
tl <- lapply(t, function(x) within(x, C <- paste(IMG.Submission.ID, Study.Name, sep=':')))
tl2 <- lapply(tl, function(x) aggregate(C~Contact.Name, x, paste, collapse="\n"))
tl3 <- lapply(tl2, function(x) x[ , "C"])
proj_info <- data.frame(matrix(unlist(tl3), nrow=length(tl3), byrow=T))
cont_list3 <- cbind(cont_list2, proj_info)
colnames(cont_list3) <- c("contact_name", "contact_email", "proj_id:study_name")
cont_list3 <- cont_list3[order(cont_list3$contact_name),] 
#write.xlsx(cont_list3, file = "permission_email_information.xlsx")
#write.csv(cont_list3, file = "permission_email_information.csv")
#write.table(cont_list3, "permission_email_information.tsv",quote=FALSE,sep='\t') )
#cont_list <- cont_list[order(cont_list$Contact.Name),] 

