#####
# Bit Score Distribution Analysis
#####

#Reading in .diamond files:
#The setwd function sets the working directory to wherever we have the diamond results saved
setwd("/srv/data/big_blast/data/diamond_results")
#Then we put all of those files into a list called file_list
file_list <- list.files(path = "/srv/data/big_blast/data/diamond_results")
#You can check out the list by doing:
head(file_list) #shows you first few files
length(file_list) #shows you how many files are in the list

#Now we use this loop to read in all of those files at the same time
#First, create an empty list called dataset 
mag_results <- list()
r <- file_list[sample(length(file_list), 1000)]

#Then, we loop through the file_list and read in each file, 
#and then store each one in a spot in the list
 for (i in 1:length(r)){
    temp_data <- data.table::fread(r[i], header = F)
    mag_results[[i]] <- temp_data
 }


setwd("/srv/data/big_blast/data/isolate_results/bacteria")
file_list <- list.files(path = "/srv/data/big_blast/data/isolate_results/bacteria")

head(file_list) 
length(file_list) 

iso_bacteria <- list()
r <- file_list[sample(length(file_list), 1000)]

for (i in 1:length(r)){
    temp_data <- data.table::fread(r[i], header = F)
    iso_bacteria[[i]] <- temp_data
}

setwd("/srv/data/big_blast/data/isolate_results/archaea")
file_list <- list.files(path = "/srv/data/big_blast/data/isolate_results/archaea")

head(file_list) 
length(file_list) 

iso_archaea <- list()

for (i in 1:length(file_list)){
    temp_data <- data.table::fread(file_list[i], header = F)
    iso_archaea[[i]] <- temp_data
}

zeros <- which(grepl("0", cols))
isolates <- isolates[-c(zeros)]
 #name columns in each df in list
 headers <- c("Query Id", "Query Length", 
              "Subject Id", "Subject Length", 
              "Percent Of Identical_matches","Alignment Length",
              "Number of mismatches", "Number of Gap Openings",
              "Total Number of gaps",  "Percentage of Positive-Scoring Matches", 
              "Start of Alignment in query", "End of Alignment in query",
              "start of Alignment in subject", "End of Alignment in subject",
               "Eval", "Bit Score")
mag_results <- lapply(mag_results, setNames, headers)
iso_bacteria <- lapply(iso_bacteria, setNames, headers)
iso_archaea <- lapply(iso_archaea, setNames, headers)

mag_results_df <- rbindlist(mag_results)
iso_bact_df <- rbindlist(iso_bacteria)
iso_arch_df <- rbindlist(iso_archaea)

sequence <- seq(1:nrow(mag_results_df))
r2_mag <- sample(sequence, 1000)
sequence2 <- seq(1:nrow(iso_bact_df))
r2_iso <- sample(sequence2, 1000)
sequence3 <- seq(1:nrow(iso_arch_df))
r3_iso <- sample(sequence3, 1000)

mag_sample <- mag_results_df[r2_mag, ]
iso_bact_sample <- iso_bact_df[r2_iso, ]
iso_arch_sample <- iso_bact_df[r3_iso, ]

dens_mags <- density(mag_sample$'Bit Score')
dens_bact <- density(iso_bact_sample$'Bit Score')
dens_arch <- density(iso_arch_sample$'Bit Score')
plot(dens_mags, main = "Density of Bit Scores per Dataset")
lines(dens_bact, col = "blue")
lines(dens_arch, col = "red")


 