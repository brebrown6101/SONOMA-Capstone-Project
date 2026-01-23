#### Script to clean SONOMA csv data ####



#### SETTINGS ####

# Put your path to the SONOMA csv here 
#(e.g. //biostat-fs2-s/users/sherold/Documents/SONOMA-Data/sonoma_raw.csv)
data_path = ""

# Put your path to where you want to save the cleaned data 
#(e.g. //biostat-fs2-s/users/sherold/Documents/SONOMA-Data/sonoma_cleaned.csv)
ouptut_path = ""




#### LOAD ####
df = read.csv(data_path) 
library(dplyr)




#### CLEAN (COMMENT ON EVERY LINE PLEASE) ####

df = df[df$eligiblefull == 1,] # Select only fully eligible observations

# etc., etc.




#### SAVE ####

write.csv(df, ouptut_path)
