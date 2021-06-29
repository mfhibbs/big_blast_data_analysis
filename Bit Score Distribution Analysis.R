#####
# Bit Score Distribution Analysis
#####

#Read in .diamond files 
setwd("/srv/data/big_blast/data/diamond_results")
 file_list <- list.files(path = "/srv/data/big_blast/data/diamond_results")
 dataset <- list()
 for (i in 1:length(file_list)){
    temp_data <- data.table::fread(file_list[i], header = F)
    dataset[[i]] <- temp_data
 }

 #name columns in each df in list
 headers <- c("Query Id", "Query Length", 
              "Subject Id", "Subject Length", 
              "Percent Of Identical_matches","Alignment Length",
              "Number of mismatches", "Number of Gap Openings",
              "Total Number of gaps",  "Percentage of Positive-Scoring Matches", 
              "Start of Alignment in query", "End of Alignment in query",
              "start of Alignment in subject", "End of Alignment in subject",
               "Eval", "Bit Score")
 bit_score <- lapply(dataset, setNames, headers)
 #bit_score <- lapply(bit_score, function(x) x[ , c(1,6)])
 
 #rbind all tables in list to create one dataframe
 library(data.table)
 bit_score_df <- rbindlist(bit_score)

 #method 1: sample a few lines from each mag 
 bit_score_samp <- lapply(bit_score, function(x) x[sample(nrow(x), 3), ])
 b_s_samp_df <- rbindlist(bit_scores_samp)
 
 #method 2: sample a total of 10,000 mags from entire dataframe
 bit_score_samp <-  bit_score_df[sample(nrow(bit_score_df), 10000), ]
 
 #For both methods, plot the density distribution 
 dens <- density(b_s_samp_df$'Bit Score')
 plot(dens, frame = FALSE, col = "steelblue", 
       main = "Density plot of bit scores", xlim = c(0, 1000)) 
 
 #method 3: Quantile analysis
 bit_scores <- bit_score_df$'Bit Score'
 bit_scores <- bit_scores[order(-bit_scores)]
 xq <- quantile(bit_scores) 
 sapply(xq, function(y)which.min(abs(bit_scores - y))) 
 q1 <- bit_scores[84596747:112798033]
 q2 <- bit_scores[56250552:84596746]
 q3 <- bit_scores[28102434 :56250551]
 
 mags_tax <- read.csv("/home/mhibbs1/big_blast_data_analysis/mags_phy.csv")
 mags_tax <- mags_tax[ , c("metagenome_id", "Phylum", "Class", "Order", "Family", "Genus", "Species")]
 gems_joined_tax <- gems_joined %>%
        left_join(mags_tax, by = c("metagenome_id"))