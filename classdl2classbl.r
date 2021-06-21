# classification <- read_excel("~/GitHub/arctos-r/input/temp_ait.xlsx") # read in classification file
classification <- read_excel("~/GitHub/arctos-r/input/temp_ichnus.xlsx") # read in classification file
classification <- classification[which(classification$SOURCE == "Arctos"),] # get only Arctos classifications

df <- classification[c("CLASSIFICATION_ID")] # create dataframe of classificationID
df <- df[which(!duplicated(df$CLASSIFICATION_ID)),] # get list of unique classification IDs
justclass <- classification[which(!is.na(classification$POSITION_IN_CLASSIFICATION)),] # get all classification terms
noclass <- classification[which(is.na(classification$POSITION_IN_CLASSIFICATION)),]
if (nrow(justclass) + nrow(noclass) == nrow(classification)){
  noclass <- noclass[which(noclass$TERM_TYPE != "display_name" & noclass$TERM_TYPE != "scientific_name"),] # remove scientific name from the list
  check <- nrow(noclass)
  
  df$username <- "jegelewicz" # add username
  df$hierarchy_name <- "Arctos Ichnology" # add hierarchy name
  
  sciname <- classification[which(classification$TERM_TYPE == "scientific_name"),] # get list of scientific names
  for (i in 1:nrow(df)){
    df$scientific_name[i] <- vlookup(classification$SCIENTIFIC_NAME,df$CLASSIFICATION_ID[i],classification$CLASSIFICATION_ID)
  } # add scientific name
  
  # non-classification terms
  used <- data.frame(CLASSIFICATION_ID=character(),
                     stringsAsFactors=FALSE) # initialize temporary dataframe
  k <- 0 # initialize variable k
  
  while (nrow(noclass) > 0){
    classes <- unique(noclass$TERM_TYPE)# get maximum number of classification terms in any given classification
    classcount <- length(classes) # get the number of unique class terms
    for (j in 1:(classcount)){
      classj <- noclass[which(noclass$TERM_TYPE == classes[[j]]),] # get all terms in classification position j
      classj1 <- classj[which(!duplicated(classj$CLASSIFICATION_ID)),] # get deduplicated for class
      used <- rbind.fill(used,classj1) # create a dataframe of used classification terms
      classtyp <- paste('noclass_term_type_', (j+k), sep="") # set class term type column number
      classterm <- paste('noclass_term_', (j+k), sep="") # set class term column number
      for (i in 1:nrow(df)){
        df[[classtyp]][i] <- vlookup(classj1$TERM_TYPE,df$CLASSIFICATION_ID[i],classj1$CLASSIFICATION_ID) # get the classification term type for the position and taxon
        df[[classterm]][i] <- vlookup(classj1$TERM,df$CLASSIFICATION_ID[i],classj1$CLASSIFICATION_ID) # get the classification term for the position and taxon
      }
    }
    noclass <- noclass[which(noclass$TAXON_TERM_ID %!in% used$TAXON_TERM),] # get the terms that haven't been added
    k <- classcount + k # iterate variable k
  }  
  
  # sanity check
  test <- (length(which(!is.na(df))) - length(which(!is.na(df$CLASSIFICATION_ID))) - length(which(!is.na(df$scientific_name))) - length(which(!is.na(df$username))) - length(which(!is.na(df$hierarchy_name))))/2
  if (check == test){
    
    # classification terms
    classes <- max(unique(justclass$POSITION_IN_CLASSIFICATION)) # get maximum number of classification terms in any given classification
    
    for (j in 0:classes){
      classj <- justclass[which(justclass$POSITION_IN_CLASSIFICATION == j),] # get all terms in classification position j
      classtyp <- paste('class_term_type_', j, sep="") # set class term type column number
      classterm <- paste('class_term_', j, sep="") # set class term column number
      for (i in 1:nrow(df)){
        df[[classtyp]][i] <- vlookup(classj$TERM_TYPE,df$CLASSIFICATION_ID[i],classj$CLASSIFICATION_ID) # get the classification term type for the position and taxon
        df[[classterm]][i] <- vlookup(classj$TERM,df$CLASSIFICATION_ID[i],classj$CLASSIFICATION_ID) # get the classification term for the position and taxon
      }
    }
    
    #sanity check
    test <- (length(which(!is.na(df))) - length(which(!is.na(df$CLASSIFICATION_ID))) - length(which(!is.na(df$scientific_name))) - length(which(!is.na(df$username))) - length(which(!is.na(df$hierarchy_name))))/2
    termcount <- classification[which(classification$TERM_TYPE != "display_name" & classification$TERM_TYPE != "scientific_name"),]
    if (nrow(termcount) == test){
    
      write.csv(df,"~/GitHub/arctos-r/output/class_load.csv", row.names = FALSE)
      } else {
        print("data was lost in classification terms")
      }
  } else {
    print("data was lost in non-classification terms")
  }
}