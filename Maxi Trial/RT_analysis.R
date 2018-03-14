
## Reaction Times Analysis 

results = list.files(pattern = ".csv")
data = lapply(results, read.csv, header = FALSE, stringsAsFactors = FALSE)
data_frame <- do.call("rbind", data)

data_frame <- data_frame[-1: -2, ] %>%  ## remove first 2 rows (completion code) 
  dplyr::select(V2, V7, V27, V28, V29, V33, V34)  ## and select relevant columns

colnames(data_frame) <- data_frame[1,]   ####### set the first row to be the header of the data frame 
final_data <- data_frame[-1, ] 

final_data <- filter(final_data, trialNo != "na", trialNo != "", trialNo != "trialNo") 

final_data <- final_data %>%  # add a column for sujb_id, 275 represents n° of trials per participant
  mutate("subj_id" = rep(1:55,each=275))   ### change 48 with actual number of participants

ultimatum <- final_data %>%   ## create a dataframe with only UltimatumGame results 
  group_by(subj_id) %>%   #### recode response as acceptance -> accepted=1, rejected = 0
  filter(game == "ultimatum") %>% 
  mutate(acceptance = if_else(response == 1, 0L, 1L)) %>% 
  dplyr::select(subj_id, everything()) 

ultimatum$RT <- as.numeric(ultimatum$RT)
ultimatum_rt <- ultimatum %>%
  group_by(subj_id, stim_type, offer) %>%
    mutate(mean_rt = mean(RT))
