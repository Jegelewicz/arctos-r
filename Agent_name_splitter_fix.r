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

df <- read.csv("C:/Users/Teresa/Dropbox/Arctos/APSU/Memphis_splitAgentNames.csv", na.strings = c("", "NA")) # read in classification file
colnames(df)[colnames(df) == "PREFERRED_NAME"] <- "name_supplied" # change header for preferred name column
df$preferred_name <- df$OTHER_NAME_1
df$OTHER_NAME_1 <- df$OTHER_NAME_2 # move name 2 to 1
df$OTHER_NAME_TYPE_1 <- df$OTHER_NAME_TYPE_2 # move name type 2 to 1
df$OTHER_NAME_2 <- df$OTHER_NAME_3 # move name 3 to 2
df$OTHER_NAME_TYPE_2 <- df$OTHER_NAME_TYPE_3 # move name type 3 to 2
df$OTHER_NAME_3 <- df$OTHER_NAME_4 # move name 4 to 3
df$OTHER_NAME_TYPE_3 <- df$OTHER_NAME_TYPE_4 # move name type 4 to 3
df$OTHER_NAME_4 <- NA # remove duplicate data
df$OTHER_NAME_TYPE_4 <- NA #remove duplicate data

df$OTHER_NAME_TYPE_1 <- ifelse(is.na(df$OTHER_NAME_1),NA,"last name") # remove type when value is blank
df$OTHER_NAME_TYPE_2 <- ifelse(is.na(df$OTHER_NAME_2),NA,"middle name") # remove type when value is blank
df$OTHER_NAME_TYPE_3 <- ifelse(is.na(df$OTHER_NAME_3),NA,"first name") # remove type when value is blank

df$OTHER_NAME_1  <- gsub("Unknown", "unknown", df$OTHER_NAME_1) # lowercase unknown in last name
df$preferred_name  <- gsub("Unknown", "unknown", df$preferred_name) # lowercase unknown in preferred name

# split any merged initials (J.L. instead of J. L.)
for(i in 1:nrow(df)){
  chars <- unlist(strsplit(df$OTHER_NAME_3[i],"")) # turn the name into a vector
  if(length(chars[chars == "."]) > 1){ # if there are more than one period in the vector, assume initials need splitting
    df$OTHER_NAME_2[i] <- right(df$OTHER_NAME_3[i],".",2) # put second initial in middle name
    df$OTHER_NAME_3[i] <- left(df$OTHER_NAME_3[i],".",1) # put first initital in first name
    df$preferred_name[i] <- paste(df$OTHER_NAME_3[i],df$OTHER_NAME_2[i],df$OTHER_NAME_1[i], sep = " ") # reformat preferred name to include space between initials
  }
}

# assume single names are last name only
for (i in 1:nrow(df)){
  chars <- unlist(strsplit(df$name_supplied[i],"")) # turn the name into a vector
  if (length(chars[chars == " "]) < 1){ # if there are no spaces in submitted_name, assume it is last name only
    df$preferred_name[i] <- df$name_supplied[i] # put supplied name in preferred name
    df$OTHER_NAME_1[i] <- df$name_supplied[i] # put supplied name in last name
    df$OTHER_NAME_TYPE_1[i] <- "last name" # add last name to type for new last name
  }
}

# create better notes
# Names in Arctos
for (i in 1:nrow(df)){
  if (grepl("FATAL ERROR: duplicate of",df$SUGGESTIONS[i], fixed = TRUE) == TRUE){ # does the Suggestion include an exact duplicate?
    df$inArctos[i] <- right(df$SUGGESTIONS[i],"FATAL ERROR: duplicate of",26) # if so, drop everything to the left of it
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
    if (grepl("possible duplicate of",df$SUGGESTIONS[i], fixed = TRUE) == TRUE){ # does the Suggestion include a possible duplicate?
      df$near_match_Arctos[i] <- right(df$SUGGESTIONS[i],"possible duplicate of",22) # if so, drop everything to the left of it
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
