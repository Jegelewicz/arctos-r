classification <- read_excel("~/GitHub/arctos-r/input/temp_cont_ichnus.xlsx") # read in classification file
# classification <- classification[which(classification$SOURCE == "Arctos"),] # get only Arctos classifications

classification$test <- paste(classification$SCIENTIFIC_NAME,classification$CLASSIFICATION_ID, sep = " ") # create unique identifier for name and classification ID combination
classification$noclass_term_type_1 <- "source_authority" # set up term type for source_authority
classification$noclass_term_1 <- paste(classification$SOURCE,classification$LASTDATE,sep = " ") # create a source plus date field for source_authority
df <- classification # change dataframe name for ease of use
df <- df[which(!duplicated(df$test)),] # get list of unique test IDs
df <- df[c("test","noclass_term_type_1","noclass_term_1")] # create dataframe of test IDs
justclass <- classification[which(!is.na(classification$POSITION_IN_CLASSIFICATION)),] # get all classification terms
noclass <- classification[which(is.na(classification$POSITION_IN_CLASSIFICATION)),]
if (nrow(justclass) + nrow(noclass) == nrow(classification)){
  noclass <- noclass[which(noclass$TERM_TYPE != "display_name" & noclass$TERM_TYPE != "scientific_name"),] # remove scientific name from the list
  check <- nrow(noclass)
  df$username <- "jegelewicz" # add username
  df$hierarchy_name <- "Arctos Ichnology" # add hierarchy name
  # sciname <- classification[which(classification$TERM_TYPE == "scientific_name"),] # get list of scientific names
  df$scientific_name <- NA # create column for scientific_name
  for (i in 1:nrow(df)){
    df$scientific_name[i] <- vlookup(classification$SCIENTIFIC_NAME,df$test[i],classification$test)
  } # add scientific name
  
  # non-classification terms
  used <- data.frame(test=character(),
                     stringsAsFactors=FALSE) # initialize temporary dataframe
  k <- 1 # initialize variable k
  
  while (nrow(noclass) > 0){
    classes <- unique(noclass$TERM_TYPE)# get maximum number of classification terms in any given classification
    classcount <- length(classes) # get the number of unique class terms
    for (j in 1:(classcount)){
      classj <- noclass[which(noclass$TERM_TYPE == classes[[j]]),] # get all terms in classification position j
      classj1 <- classj[which(!duplicated(classj$test)),] # get deduplicated for class
      used <- rbind.fill(used,classj1) # create a dataframe of used classification terms
      classtyp <- paste('noclass_term_type_', (j+k), sep="") # set class term type column number
      classterm <- paste('noclass_term_', (j+k), sep="") # set class term column number
      for (i in 1:nrow(df)){
        df[[classtyp]][i] <- vlookup(classj1$TERM_TYPE,df$test[i],classj1$test) # get the classification term type for the position and taxon
        df[[classterm]][i] <- vlookup(classj1$TERM,df$test[i],classj1$test) # get the classification term for the position and taxon
      }
    }
    noclass <- noclass[which(noclass$TAXON_TERM_ID %!in% used$TAXON_TERM),] # get the terms that haven't been added
    k <- classcount + k # iterate variable k
  }  
  
  # sanity check
  test <- (length(which(!is.na(df))) - length(which(!is.na(df$test))) - length(which(!is.na(df$scientific_name))) - length(which(!is.na(df$username))) - length(which(!is.na(df$hierarchy_name))) - length(which(!is.na(df$noclass_term_type_1))) - length(which(!is.na(df$noclass_term_1))))/2
  if (check == test){
    
    # classification terms
    check2 <- nrow(justclass)
    used <- data.frame(CLASSIFICATION_ID=character(),
                       stringsAsFactors=FALSE) # initialize temporary dataframe
    k <- 0 # initialize variable k
    while (nrow(justclass) > 0){
      # classes <- max(unique(justclass$POSITION_IN_CLASSIFICATION)) # get maximum number of classification terms in any given classification
      classes <- unique(justclass$POSITION_IN_CLASSIFICATION)# get maximum number of classification terms in any given classification
      classcount <- length(classes) # get the number of unique class terms
      for (j in 0:classcount){
        classj <- justclass[which(justclass$POSITION_IN_CLASSIFICATION == j),] # get all terms in classification position j
        classj1 <- classj[which(!duplicated(classj$test)),] # get deduplicated for class
        classtyp <- paste('class_term_type_', (j+k), sep="") # set class term type column number
        classterm <- paste('class_term_', (j+k), sep="") # set class term column number
        used <- rbind.fill(used,classj1) # create a dataframe of used classification terms
        for (i in 1:nrow(df)){
          df[[classtyp]][i] <- vlookup(classj1$TERM_TYPE,df$test[i],classj1$test) # get the classification term type for the position and taxon
          df[[classterm]][i] <- vlookup(classj1$TERM,df$test[i],classj1$test) # get the classification term for the position and taxon
        }
      }
      justclass <- justclass[which(justclass$TAXON_TERM_ID %!in% used$TAXON_TERM_ID),] # get duplicate ranks by CLASSIFICATION_ID
      k <- k + classcount
    }
    
    #sanity check
    test <- (length(which(!is.na(df))) - length(which(!is.na(df$test))) - length(which(!is.na(df$username))) - length(which(!is.na(df$scientific_name))) - length(which(!is.na(df$hierarchy_name))))- length(which(!is.na(df$noclass_term_type_1))) - length(which(!is.na(df$noclass_term_1)))
    test2 <- (length(which(!is.na(classification$TERM))) + length(which(!is.na(classification$TERM_TYPE))) - (length(which(classification$TERM_TYPE == "scientific_name"))*2) - (length(which(classification$TERM_TYPE == "display_name"))*2))
    if (test2 == test){
      write.csv(df,"~/GitHub/arctos-r/output/class_load.csv", row.names = FALSE)
      print("no data was lost")
      } else {
        print("data was lost in classification terms")
      }
  } else {
    print("data was lost in non-classification terms")
  }
}
