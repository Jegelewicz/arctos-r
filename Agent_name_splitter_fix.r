# spiffy up Agent Name Splitter Results

# define function: right
right = function (string, char, n) {
  substr(string,(unlist(lapply(gregexpr(pattern = char, string), min)) + n),nchar(string))
}

# define function: left
left = function (string,char, n) {
  substr(string,1,unlist(lapply(gregexpr(pattern = char, string), min)) + n)
}

# define function: name length
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)

# define function VLOOKUP (x = return value dataframe and column,
# y = lookup value dataframe and column, z = lookup dataframe and column
# x and z should be from the same dataframe)
vlookup <- function(x,y,z){
  x[match(y,z)]
}

df <- read.csv("C:/Users/Teresa/Dropbox/Arctos/APSU/Memphis_splitAgentNames.csv", na.strings = c("", "NA")) # read in classification file

# transform column headers
colnames(df) <- tolower(colnames(df)) # lower case column names

colnames(df)[colnames(df) == "preferred_name"] <- "name_supplied" # change header for preferred name column
df$preferred_name <- df$other_name_1
df$other_name_1 <- df$other_name_2 # move name 2 to 1
df$other_name_type_1 <- df$other_name_type_2 # move name type 2 to 1
df$other_name_2 <- df$other_name_3 # move name 3 to 2
df$other_name_type_2 <- df$other_name_type_3 # move name type 3 to 2
df$other_name_3 <- df$other_name_4 # move name 4 to 3
df$other_name_type_3 <- df$other_name_type_4 # move name type 4 to 3
df$other_name_4 <- NA # remove duplicate data
df$other_name_type_4 <- NA #remove duplicate data

df$other_name_type_1 <- ifelse(is.na(df$other_name_1),NA,"last name") # remove type when value is blank
df$other_name_type_2 <- ifelse(is.na(df$other_name_2),NA,"middle name") # remove type when value is blank
df$other_name_type_3 <- ifelse(is.na(df$other_name_3),NA,"first name") # remove type when value is blank

df$other_name_1  <- gsub("Unknown", "unknown", df$other_name_1) # lowercase unknown in last name
df$preferred_name  <- gsub("Unknown", "unknown", df$preferred_name) # lowercase unknown in preferred name

df$agent_type <- "person" # add agent type for bulkloader

# split any merged initials (J.L. instead of J. L.)
for(i in 1:nrow(df)){
  chars <- unlist(strsplit(df$other_name_3[i],"")) # turn the name into a vector
  if(length(chars[chars == "."]) > 1){ # if there are more than one period in the vector, assume initials need splitting
    df$other_name_2[i] <- right(df$other_name_3[i],".",2) # put second initial in middle name
    df$other_name_3[i] <- left(df$other_name_3[i],".",1) # put first initital in first name
    df$preferred_name[i] <- paste(df$other_name_3[i],df$other_name_2[i],df$other_name_1[i], sep = " ") # reformat preferred name to include space between initials
  }
}

# assume single names are last name only
for (i in 1:nrow(df)){
  chars <- unlist(strsplit(df$name_supplied[i],"")) # turn the name into a vector
  if (length(chars[chars == " "]) < 1){ # if there are no spaces in submitted_name, assume it is last name only
    df$preferred_name[i] <- df$name_supplied[i] # put supplied name in preferred name
    df$other_name_1[i] <- df$name_supplied[i] # put supplied name in last name
    df$other_name_type_1[i] <- "last name" # add last name to type for new last name
  }
}

# create better notes
# Names in Arctos
for (i in 1:nrow(df)){
  if (grepl("FATAL ERROR: duplicate of",df$suggestions[i], fixed = TRUE) == TRUE){ # does the Suggestion include an exact duplicate?
    df$inArctos[i] <- right(df$suggestions[i],"FATAL ERROR: duplicate of",26) # if so, drop everything to the left of it
    if (grepl(";",df$inArctos[i],fixed = TRUE) == TRUE){ # is there a semicolon after the duplicate information in the suggestion?
      df$inArctos[i] <- left(df$inArctos[i],";",1) # if so, drop everything to the right of it
    } else {
      df$inArctos[i] <- df$inArctos[i] # if not, leave inArctos as is
    }
  } else {
    df$inArctos[i] <- NA # if there is no exact match in the suggestion, inArctos will be NA
  }
}

# Near matches in Arctos
for (i in 1:nrow(df)){
  if (is.na(df$inArctos[i])){ # if there is not an exact match in suggestion
    if (grepl("possible duplicate of",df$suggestions[i], fixed = TRUE) == TRUE){ # does the Suggestion include a possible duplicate?
      df$near_match_Arctos[i] <- right(df$suggestions[i],"possible duplicate of",22) # if so, drop everything to the left of it
    } else {
      df$near_match_Arctos[i] <- NA # if there is no possible match in the suggestion, near_match_Arctos will be NA
    }
  } else {
    df$near_match_Arctos[i] <- NA # if there is an exact match in the suggestion, near_match_Arctos will be NA
  }
}

# Flags
for (i in 1:nrow(df)){
  if (name_length(df$preferred_name[i]) < 2){
    df$flag[i] <- "short name"
  } else {
    df$flag[i] <- NA
  }
}

# check Levenshtein's Distance (e.g., misspellings) there can be NO duplicates in the column to be evaluated
# Watch for: Smith; Smit; Schmit
library(stringdist)
temp <- c()
similar_names <-c()
compared_names <- c()
cutoff_distance <- 2
df2 <- c()
io <- FALSE
for(i in 1:length(df$preferred_name)){
  if(!(df$preferred_name[i] %in% similar_names)){ # testing
    for(j in 1:length(df$preferred_name)){
      score <- stringdist(df$preferred_name[i], df$preferred_name[j], "dl")
      temp <- c(temp, score)
    }
    if(any(temp %in% c(1:cutoff_distance))){
      if(io){
        df2 <- cbind(df2, temp)
        wc = wc + 1
      } else {
        df2 <- as.data.frame(temp)
        rownames(df2) <- df$preferred_name
        io <- TRUE
        wc <- 1
      }
      colnames(df2)[which(colnames(df2) == "temp")] <- df$preferred_name[i]
      similar <- rownames(df2)[which(df2[,wc]==min(df2[,wc][which(df2[,wc]>0)]))]
      comp_name <- rep(df$preferred_name[i], length(similar))
      similar_names <- c(similar_names, similar)
      compared_names <- c(compared_names, comp_name)
    }
    temp <- c()
  }
  if(i %% 10 == 0){
    print(paste('Completed iteration:', i, 'out of', length(df$canonicalName), 'iterations (', round(i/length(df$canonicalName),2)*100,'% DONE)'))
  }
}
print('FINISHED!')
check_mat <- as.data.frame(cbind(compared_names, similar_names)) # create matched names data frame
df$similar_name <- vlookup(check_mat$similar_names,df$preferred_name,check_mat$compared_names) # place similar compared names in working sheet
df$similar_name <- ifelse(is.na(df$similar_name),vlookup(check_mat$compared_names,df$preferred_name,check_mat$similar_names),df$similar_name) # place similar similar names in working sheet

# order columns
# df[,c(1,2,3,4)]. Note the first comma means keep all the rows, and the 1,2,3,4 refers to the columns.
df <- df[,c("agent_type",
            "preferred_name",
            "other_name_1",
            "other_name_type_1",
            "other_name_2",
            "other_name_type_2",
            "other_name_3",
            "other_name_type_3",
            "other_name_4",
            "other_name_type_4",
            "agent_remark",
            "name_supplied",
            "similar_name",
            "inArctos",
            "near_match_Arctos",
            "flag",
            "suggestions"
)]

# sort for ease of use
df <- df[with(df, order(other_name_1)), ]

write.csv(df,"~/GitHub/arctos-r/output/agent_names_split.csv", row.names = FALSE) # write out names for review
            