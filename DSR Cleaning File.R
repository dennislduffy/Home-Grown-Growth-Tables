#This file takes raw DSR files from the past 5 years, combines them, and creates percentile growth charts for ACCESS test scores.
#When running this file you must first check that all tables in the database GrowthTableDatabase are deleted. 


# R version 3.2.3 (2015-12-10)
# Platform: i386-w64-mingw32/i386 (32-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] RODBC_1.3-13 dplyr_0.4.3 
# 
# loaded via a namespace (and not attached):
#   [1] magrittr_1.5   R6_2.1.2       assertthat_0.1 parallel_3.2.3 DBI_0.3.1      tools_3.2.3    Rcpp_0.12.3  

#clear the workspace 
rm(list = ls())

#Check if packages are install and then install any packages needed to run program
packages <- c("dplyr", "RODBC", "sets")

for(package in packages){
  
  if(package %in% rownames(installed.packages()) == F){
    
    install.packages(package)
    
  }
  
}

#set up dplyr and RODBC
x = c("dplyr", "RODBC")

lapply(x, library, character.only = T)

#set up home working directory
homedir <- "//EDU-FileServer/HomeDirs/dduffy/My Documents/Data/State DSRs/State DSR/MN Home Brewed Growth Tables/Growth Tables Files"

#open database connection
db <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=//EDU-FileServer/HomeDirs/dduffy/My Documents/Data/State DSRs/State DSR/MN Home Brewed Growth Tables/Growth Tables Files/GrowthTableDatabase.accdb")

#define trim function
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#create translation table for domains
domain.codes <- data.frame(subject = c("C", "A", "W", "O", "R"),
                           DOMAIN = c("COMPOSITE", "LISTENING", "WRITING", "SPEAKING", "READING"))


#create translation table for proficiency levels 
proficiency.key <- data.frame(Proficiency.Key = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"), 
                              Proficiency = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6))


#Make a grade key. Note: Grades 9-12 are classified as HS because WIDA lumps those grades together for the growth
#tables. 
LYGradeKey <- data.frame(LYGradeKey = c("KG", "FIRST", "SECOND", "THIRD", "FOURTH", "FIFTH", "SIXTH", "SEVENTH", "EIGHTH", 
                                        rep("HS",4)), 
                         grade.level = c("KG", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))

#set working directory
setwd(paste(homedir, "/Raw DSR Files", sep = ""))

#list all folders in the directory
folder.list <- as.list(list.files())

for(folder in folder.list){
  
  setwd(paste(homedir, "/Raw DSR Files/", folder, sep = ""))

  #create a list of all text files in the working directory
  file.list = as.list(list.files(pattern="*.txt"))
  
  #open and clean all files
  for(file in file.list){
    pos <- match(file, file.list)
    tempfile.name <- as.character(file.list[pos])
    
    #load file 
    tempfile.raw <- read.table(file = tempfile.name, sep = "\t", quote = "\"", header = T
                               , colClasses = c("marssNumber" = "character"
                                                , "marssLocalUseData" = "character" 
                                                ,"testDate" = "character"
                                                , "districtNumber" = "character"
                                                , "districtType" = "character"
                                                , "schoolNumber" = "character"))
    
    
    tempfile.raw$districtNumber<- trim(tempfile.raw$districtNumber)
    tempfile.raw$districtType <- trim(tempfile.raw$districtType)
    tempfile.raw$schoolNumber <- trim(tempfile.raw$schoolNumber)
    
    
    tempfile <- tempfile.raw %>%
      select(marssNumber, marssLocalUseData, testDate, districtNumber, districtType, schoolNumber, scoreCode, grade, 
             lepFlag, score1, score2, subject, admSince97) %>%
      filter(scoreCode == "VS") %>%
      mutate(score1 = score1/100, 
             score2 = score2/100, 
             SCHOOLID = paste(districtNumber, districtType, schoolNumber, sep = "")) %>%
      mutate(YEAR = substr(testDate, 1, 4)) %>% 
      rename(MARSSID = marssNumber, 
             LOCALSTUDENTID = marssLocalUseData, 
             GRADE = grade, 
             SCORE1 = score1, 
             SCORE2 = score2, 
             ADM = admSince97) %>%
      left_join(domain.codes, by = "subject") %>%
      select(-c(subject, testDate, districtNumber, districtType, schoolNumber, scoreCode, lepFlag))
    
    #create file name
    file.name <- paste(paste(unique(tempfile$DOMAIN), unique(tempfile$YEAR), sep = "_"), ".rds", sep = "")
    
    saveRDS(tempfile, file.name)
  }
  
    YEAR <- unique(tempfile$YEAR)
    
    #read in .rds files
    rds.files <- list.files(pattern = "*.rds")
    
    #bind .rds files together to make the combined DSR
    temp.DSR <- data.frame(data.table::rbindlist(lapply(rds.files, readRDS)))
    
    temp.DSR <- temp.DSR %>%
      filter(SCORE2 > 0)
    
    temp.DSR$AnalysisID <- ifelse(temp.DSR$MARSSID == "" & temp.DSR$LOCALSTUDENTID != "", temp.DSR$LOCALSTUDENTID, temp.DSR$MARSSID)
    
    temp.DSR <- filter(temp.DSR, AnalysisID != "")
    
    #Remove duplicates
    temp.DSR <- temp.DSR %>%
      arrange(AnalysisID, DOMAIN) 
    
    ID.Remove <- temp.DSR %>%
      group_by(AnalysisID) %>%
      filter(n() > 5) %>%
      select(AnalysisID)
    
    temp.DSR <- temp.DSR[!(temp.DSR$AnalysisID %in% ID.Remove$AnalysisID), ]
    
    #Remove students with fewer than 5 scores
    temp.DSR <- temp.DSR %>%
      arrange(AnalysisID, DOMAIN) 
    
    ID.Remove <- temp.DSR %>%
      group_by(AnalysisID) %>%
      filter(n() < 5) %>%
      select(AnalysisID)
    
    temp.DSR <- temp.DSR[!(temp.DSR$AnalysisID %in% ID.Remove$AnalysisID), ]
    
    temp.DSR <- temp.DSR %>%
      select(AnalysisID, GRADE, SCORE1, SCORE2, DOMAIN) %>%
      rename(ScaleScore = SCORE1, 
             ProficiencyScore = SCORE2) %>%
      mutate(ProficiencyLevel = plyr::round_any(ProficiencyScore, .5, floor))
    
    file.name <- paste("DSR_", YEAR, sep = "")
    
    sqlSave(db, temp.DSR, tablename = file.name, append = F, rownames = F)
    
}

#Make a collection of tuples with the DSRs that need to be combined
DSR_TUPLE_13 <- sets::tuple("DSR_2012", "DSR_2013")
DSR_TUPLE_14 <- sets::tuple("DSR_2013", "DSR_2014")
DSR_TUPLE_15 <- sets::tuple("DSR_2014", "DSR_2015")
DSR_TUPLE_16 <- sets::tuple("DSR_2015", "DSR_2016")

tuple.set <- sets::set("COMBINED_2013" = DSR_TUPLE_13, 
                 "COMBINED_2014" = DSR_TUPLE_14, 
                 "COMBINED_2015" = DSR_TUPLE_15, 
                 "COMBINED_2016" = DSR_TUPLE_16)

tuple.names <- c("COMBINED_2013", "COMBINED_2014", "COMBINED_2015", "COMBINED_2016")

count <- 1

for(tuple.use in tuple.set){
  
  name.use <- tuple.names[count]
  
  tuple.use <- tuple.set[[name.use]]
  
  LY_NAME <- as.character(tuple.use[1])
  CY_NAME <- as.character(tuple.use[2])
  
  LY_FILE <- sqlQuery(db, paste("SELECT * FROM ", LY_NAME, sep = ""), as.is = T)
  CY_FILE <- sqlQuery(db, paste("SELECT * FROM ", CY_NAME, sep = ""), as.is = T)
  
  #rename columns 
  LY_FILE <- rename(LY_FILE, 
                    LYGRADE = GRADE, 
                    LYScaleScore = ScaleScore, 
                    LYProficiencyScore = ProficiencyScore, 
                    LYProficiencyLevel = ProficiencyLevel)
  
  CY_FILE <- rename(CY_FILE, 
                    CYGRADE = GRADE, 
                    CYScaleScore = ScaleScore, 
                    CYProficiencyScore = ProficiencyScore, 
                    CYProficiencyLevel = ProficiencyLevel)
  
  student.data <- CY_FILE %>%
    inner_join(LY_FILE, by = c("AnalysisID", "DOMAIN")) %>%
    mutate(scorediff = CYScaleScore - LYScaleScore, 
           YEAR = substr(CY_NAME, 5, 8))
  
  #subset student to only include columns needed for growth table calculations
  student.data <- student.data %>%
    select(LYGRADE, CYGRADE, scorediff, LYProficiencyLevel, scorediff, YEAR, DOMAIN) %>%
    left_join(proficiency.key, by = c("LYProficiencyLevel" = "Proficiency")) %>%
    left_join(LYGradeKey, by = c("LYGRADE" = "grade.level")) %>%
    mutate(LYGradeKey = as.character(LYGradeKey), 
           DOMAIN = as.character(DOMAIN)) %>%
    select(-c(LYGRADE))
  
  sqlSave(db, student.data, tablename = name.use, append = F, rownames = F)
  
  count <- count + 1
}


#Make list of grades, domains, and proficiency levels
GRADES <- as.character(unique(LYGradeKey$LYGradeKey))
DOMAINS <- as.character(unique(student.data$DOMAIN))
YEAR <- unique(student.data$YEAR)
proficiency.cuts <- unique(student.data$LYProficiencyLevel)

quantile.labels <- c("<20%", ">20% & <40%", ">40% & <60%", ">60% & <80%", ">80%")
quantile.names <- c(0, 20, 40, 60, 80)
quantile.table <- data.frame(quantile.labels, quantile.names)

#Create tuples for quintile loop
TUPLE.2013 <- sets::tuple("COMBINED_2013", "2013")
TUPLE.2014 <- sets::tuple("COMBINED_2014", "2014")
TUPLE.2015 <- sets::tuple("COMBINED_2015", "2015")
TUPLE.2016 <- sets::tuple("COMBINED_2016", "2016")

#combine into a set
tuple.set <- sets::set("TUPLE.2013" = TUPLE.2013, 
                       "TUPLE.2014" = TUPLE.2014, 
                       "TUPLE.2015" = TUPLE.2015, 
                       "TUPLE.2016" = TUPLE.2016)

tuple.names <- c("TUPLE.2013", "TUPLE.2014", "TUPLE.2015", "TUPLE.2016")

#set count
count <- 1

for (tuple.use in tuple.set){
  
  name.use <- tuple.names[count]
  
  tuple.use <- tuple.set[[name.use]]
  
  data.name <- as.character(tuple.use[1])
  
  YEAR <- as.character(tuple.use[2])
  
  student.data <- sqlQuery(db, paste("SELECT * FROM ", data.name, sep = ""), as.is = T)

  setwd(paste(homedir, "/Growth Tables/", YEAR, " Tables", sep = ""))
  
  for (GRADE.USE in GRADES){
    
    for (DOMAIN.USE in DOMAINS){
      
      for (CUT in proficiency.cuts){
        
        data.use <- student.data %>%
          filter(LYGradeKey == GRADE.USE) %>%
          filter(DOMAIN == DOMAIN.USE) %>%
          filter(LYProficiencyLevel == CUT)
        
        if(nrow(data.use) >= 20){
          
          quantile.cuts <- append(as.vector(quantile(data.use$scorediff, c(.2, .4, .6, .8), T, 3)), -999, after = 0)
          
          quantile.cuts <- plyr::round_any(quantile.cuts, 1, round)
          
          quantiles <- data.frame(quantile.labels, quantile.cuts)  
          quantiles <- quantiles %>%
            rename(Scale_Growth = quantile.cuts, 
                   Percentile_Key = quantile.labels) %>%
            mutate(Proficiency = CUT, 
                   Year = YEAR, 
                   Grade = GRADE.USE, 
                   DOMAIN = DOMAIN.USE) %>%
            left_join(proficiency.key, by = "Proficiency") %>%
            left_join(quantile.table, by = c("Percentile_Key" = "quantile.labels")) %>%
            mutate(Range_Key = paste(Grade, DOMAIN, Proficiency.Key, Year, sep = "_")) %>%
            rename(Proficiency_Key = Proficiency.Key,
                   Percentile = quantile.names) %>%
            select(Scale_Growth, Percentile, Grade, DOMAIN, Proficiency_Key, Proficiency, Year, Percentile_Key, Range_Key)
          
          percentile_80_count <- length(data.use$scorediff[data.use$scorediff >= quantiles$Scale_Growth[quantiles$Percentile == 80]])
          percentile_60_count <- length(data.use$scorediff[data.use$scorediff >= quantiles$Scale_Growth[quantiles$Percentile == 60] & data.use$scorediff < quantiles$Scale_Growth[quantiles$Percentile == 80]])
          percentile_40_count <- length(data.use$scorediff[data.use$scorediff >= quantiles$Scale_Growth[quantiles$Percentile == 40] & data.use$scorediff < quantiles$Scale_Growth[quantiles$Percentile == 60]])
          percentile_20_count <- length(data.use$scorediff[data.use$scorediff >= quantiles$Scale_Growth[quantiles$Percentile == 20] & data.use$scorediff < quantiles$Scale_Growth[quantiles$Percentile == 40]])
          percentile_0_count <- length(data.use$scorediff[data.use$scorediff < quantiles$Scale_Growth[quantiles$Percentile == 20]])
          
          #create variable to hold the counts
          quantiles <- mutate(quantiles, student_count = NA)
          quantiles$student_count[quantiles$Percentile == 0] <- percentile_0_count
          quantiles$student_count[quantiles$Percentile == 20] <- percentile_20_count
          quantiles$student_count[quantiles$Percentile == 40] <- percentile_40_count
          quantiles$student_count[quantiles$Percentile == 60] <- percentile_60_count
          quantiles$student_count[quantiles$Percentile == 80] <- percentile_80_count
          
          file.name <- paste(GRADE.USE, DOMAIN.USE, CUT, YEAR, sep = "_")
          
          saveRDS(quantiles, paste(file.name, ".rds", sep = ""))
          
        }
        
        else{
          
          quantile.cuts <- c(-999, NA, NA, NA, NA)
          
          quantiles <- data.frame(quantile.labels, quantile.cuts)  
          quantiles <- quantiles %>%
            rename(Scale_Growth = quantile.cuts, 
                   Percentile_Key = quantile.labels) %>%
            mutate(Proficiency = CUT, 
                   Year = YEAR, 
                   Grade = GRADE.USE, 
                   DOMAIN = DOMAIN.USE) %>%
            left_join(proficiency.key, by = "Proficiency") %>%
            left_join(quantile.table, by = c("Percentile_Key" = "quantile.labels")) %>%
            mutate(Range_Key = paste(Grade, DOMAIN, Proficiency.Key, Year, sep = "_")) %>%
            rename(Proficiency_Key = Proficiency.Key,
                   Percentile = quantile.names) %>%
            select(Scale_Growth, Percentile, Grade, DOMAIN, Proficiency_Key, Proficiency, Year, Percentile_Key, Range_Key) %>%
            mutate(student_count = NA)
          
          
          file.name <- paste(GRADE.USE, DOMAIN.USE, CUT, YEAR, sep = "_")
          
          saveRDS(quantiles, paste(file.name, ".rds", sep = ""))
          
        }
        
      }
      
    }
    
  }
  
  count <- count + 1
  
}  

#Combine files to create growth tables

YEARS <- c("2013", "2014", "2015", "2016")

for(YEAR in YEARS){

  setwd(paste(homedir, "/Growth Tables/", YEAR, " Tables", sep = ""))  
  
  #read in all resulting .rds files from working directory, append them together, and sort them
  
  raw.growth.tables <- as.list(list.files())
  
  growth.table <- data.frame(data.table::rbindlist(lapply(raw.growth.tables, readRDS)))
  
  growth.table <- growth.table %>%
    arrange(Grade, DOMAIN, Proficiency_Key, Scale_Growth, desc(Percentile)) 
  
  sqlSave(db, growth.table, tablename = paste(YEAR, "_Minnesota_EL_Growth", sep = ""), rownames = F, append = F)
  
  #save file as .txt file
  setwd(paste(homedir, "/Growth Tables/Finished Growth Tables", sep = ""))
  
  write.table(growth.table, paste(YEAR, " Minnesota EL Growth.txt", sep = ""), sep = "\t", na = "", row.names = F)
  
}

#recreate the tables with the ntile function

#set count
count <- 1

for (tuple.use in tuple.set){
  
  name.use <- tuple.names[count]
  
  tuple.use <- tuple.set[[name.use]]
  
  data.name <- as.character(tuple.use[1])
  
  YEAR <- as.character(tuple.use[2])
  
  student.data <- sqlQuery(db, paste("SELECT * FROM ", data.name, sep = ""), as.is = T)
  
  setwd(paste(homedir, "/Growth Tables/", YEAR, " Tables", sep = ""))

  for (GRADE.USE in GRADES){
    
    for (DOMAIN.USE in DOMAINS){
      
      for (CUT in proficiency.cuts){
        
        data.use <- student.data %>%
          filter(LYGradeKey == GRADE.USE) %>%
          filter(DOMAIN == DOMAIN.USE) %>%
          filter(LYProficiencyLevel == CUT)
        
        if(nrow(data.use) >= 20){
          
          data.use$quantile.cuts <- ntile(data.use$scorediff, 5)
          
          quantile.cuts <- c(-999, min(data.use$scorediff[data.use$quantile.cuts == 2]), 
                             min(data.use$scorediff[data.use$quantile.cuts == 3]), 
                             min(data.use$scorediff[data.use$quantile.cuts == 4]),
                             min(data.use$scorediff[data.use$quantile.cuts == 5]))
          
          quantiles <- data.frame(quantile.labels, quantile.cuts)
          
          quantiles <- quantiles %>%
            rename(Scale_Growth = quantile.cuts, 
                   Percentile_Key = quantile.labels) %>%
            mutate(Proficiency = CUT, 
                   Year = YEAR, 
                   Grade = GRADE.USE, 
                   DOMAIN = DOMAIN.USE) %>%
            left_join(proficiency.key, by = "Proficiency") %>%
            left_join(quantile.table, by = c("Percentile_Key" = "quantile.labels")) %>%
            mutate(Range_Key = paste(Grade, DOMAIN, Proficiency.Key, Year, sep = "_")) %>%
            rename(Proficiency_Key = Proficiency.Key,
                   Percentile = quantile.names) %>%
            select(Scale_Growth, Percentile, Grade, DOMAIN, Proficiency_Key, Proficiency, Year, Percentile_Key, Range_Key)
          
          percentile_80_count <- length(data.use$scorediff[data.use$quantile.cuts == 5])
          percentile_60_count <- length(data.use$scorediff[data.use$quantile.cuts == 4])
          percentile_40_count <- length(data.use$scorediff[data.use$quantile.cuts == 3])
          percentile_20_count <- length(data.use$scorediff[data.use$quantile.cuts == 2])
          percentile_0_count <- length(data.use$scorediff[data.use$quantile.cuts == 1])
          
          #create variable to hold the counts
          quantiles <- mutate(quantiles, student_count = NA)
          quantiles$student_count[quantiles$Percentile == 0] <- percentile_0_count
          quantiles$student_count[quantiles$Percentile == 20] <- percentile_20_count
          quantiles$student_count[quantiles$Percentile == 40] <- percentile_40_count
          quantiles$student_count[quantiles$Percentile == 60] <- percentile_60_count
          quantiles$student_count[quantiles$Percentile == 80] <- percentile_80_count
          
          file.name <- paste(GRADE.USE, DOMAIN.USE, CUT, YEAR, sep = "_")
          
          saveRDS(quantiles, paste(file.name, ".rds", sep = ""))
          
        }
        
        else{
          
          quantile.cuts <- c(-999, NA, NA, NA, NA)
          
          quantiles <- data.frame(quantile.labels, quantile.cuts)  
          quantiles <- quantiles %>%
            rename(Scale_Growth = quantile.cuts, 
                   Percentile_Key = quantile.labels) %>%
            mutate(Proficiency = CUT, 
                   Year = YEAR, 
                   Grade = GRADE.USE, 
                   DOMAIN = DOMAIN.USE) %>%
            left_join(proficiency.key, by = "Proficiency") %>%
            left_join(quantile.table, by = c("Percentile_Key" = "quantile.labels")) %>%
            mutate(Range_Key = paste(Grade, DOMAIN, Proficiency.Key, Year, sep = "_")) %>%
            rename(Proficiency_Key = Proficiency.Key,
                   Percentile = quantile.names) %>%
            select(Scale_Growth, Percentile, Grade, DOMAIN, Proficiency_Key, Proficiency, Year, Percentile_Key, Range_Key) %>%
            mutate(student_count = NA)
          
          
          file.name <- paste(GRADE.USE, DOMAIN.USE, CUT, YEAR, sep = "_")
          
          saveRDS(quantiles, paste(file.name, ".rds", sep = ""))
          
        }
        
        
      }
      
    }
    
  }

  count <- count + 1
  
} 

YEARS <- c("2013", "2014", "2015", "2016")

for(YEAR in YEARS){
  
  setwd(paste(homedir, "/Growth Tables/", YEAR, " Tables", sep = ""))  
  
  #read in all resulting .rds files from working directory, append them together, and sort them
  
  raw.growth.tables <- as.list(list.files())
  
  growth.table <- data.frame(data.table::rbindlist(lapply(raw.growth.tables, readRDS)))
  
  growth.table <- growth.table %>%
    arrange(Grade, DOMAIN, Proficiency_Key, Scale_Growth, desc(Percentile)) 
  
  sqlSave(db, growth.table, tablename = paste(YEAR, " Minnesota EL Growth with Ntile"), rownames = F, append = F)
  
  #save file as .txt file
  setwd(paste(homedir, "/Growth Tables/Ntile Tables", sep = ""))
  
  write.table(growth.table, paste(YEAR, " Minnesota EL Growth with Ntile.txt", sep = ""), sep = "\t", na = "", row.names = F)
  
}


#close connection
odbcClose(db)




