classification <- read_excel("~/GitHub/arctos-r/input/temp_ait.xlsx") # read in classification file

df <- classification[c("CLASSIFICATION_ID")] # create dataframe of classificationID
df <- df[which(!duplicated(df$CLASSIFICATION_ID)),] # get list of unique classification IDs
noclass <- classification[which(is.na(classification$POSITION_IN_CLASSIFICATION)),]
noclass <- noclass[which(noclass$TERM_TYPE != "display_name"),] # remove display name from the list
# term_types <- unique(noclass$TERM_TYPE)

df$username <- "jegelewicz" # add username
df$hierarchy_name <- "Arctos Ichnology" # add hierarchy name

sciname <- classification[which(classification$TERM_TYPE == "scientific_name"),] # get list of scientific names
for (i in 1:nrow(df)){
  df$scientific_name[i] <- vlookup(classification$SCIENTIFIC_NAME,df$CLASSIFICATION_ID[i],classification$CLASSIFICATION_ID)
} # add scientific name

termno <- 1

author <- classification[which(classification$TERM_TYPE == "author_text"),] # get list of taxon authors
for (i in 1:nrow(df)){
  df$noclass_term_type_1 <- vlookup(author$TERM_TYPE,df$CLASSIFICATION_ID[i],author$CLASSIFICATION_ID) # add author text term type
  df$noclass_term_1[i] <- vlookup(author$TERM,df$CLASSIFICATION_ID[i],author$CLASSIFICATION_ID)
} # add author name

#sanity check
if (nrow(author) == sum(!is.na(df$noclass_term_1))){
  print("yay")
  } else {
  print ("problem")
}

source <- classification[which(classification$TERM_TYPE == "source_authority"),] # get list of taxon sources
source1 <- source[which(!duplicated(source$CLASSIFICATION_ID)),] # get deduplicated source by ID
for (i in 1:nrow(df)){
  df$noclass_term_type_2[i] <- vlookup(source1$TERM_TYPE,df$CLASSIFICATION_ID[i],source1$CLASSIFICATION_ID)
  df$noclass_term_2[i] <- vlookup(source1$TERM,df$CLASSIFICATION_ID[i],source1$CLASSIFICATION_ID)
} # add taxon source

if (nrow(source) > nrow(source1)){
  source2 <- source[which(source$TAXON_TERM_ID %!in% source1$TAXON_TERM_ID),] # get source not in source1 by ID
  for (i in 1:nrow(df)){
    df$noclass_term_type_3[i] <- vlookup(source2$TERM_TYPE,df$CLASSIFICATION_ID[i],source2$CLASSIFICATION_ID)
    df$noclass_term_3[i] <- vlookup(source2$TERM,df$CLASSIFICATION_ID[i],source2$CLASSIFICATION_ID)
  } # add taxon source
}

if (nrow(source) > (nrow(source1) + nrow(source2))){
  source3 <- source[which(source$TAXON_TERM_ID %!in% source1$TAXON_TERM_ID | source2$TAXON_TERM_ID),] # get source not in source1 or source 2 by ID
  for (i in 1:nrow(df)){
    df$noclass_term_type_4[i] <- vlookup(source3$TERM_TYPE,df$CLASSIFICATION_ID[i],source3$CLASSIFICATION_ID)
    df$noclass_term_4[i] <- vlookup(source3$TERM,df$CLASSIFICATION_ID[i],source3$CLASSIFICATION_ID)
  } else {
    print("all sourcees added")
  }
}

#sanity check
if (nrow(source) == sum(!is.na(df$noclass_term_2)) + sum(!is.na(df$noclass_term_3)) + sum(!is.na(df$noclass_term_4))){
  print("yay")
} else {
  print ("problem")
}

status <- classification[which(classification$TERM_TYPE == "taxon_status"),] # get list of taxon sources
status1 <- status[which(!duplicated(status$CLASSIFICATION_ID)),] # get deduplicated status by ID
for (i in 1:nrow(df)){
  df$noclass_term_type_5[i] <- vlookup(status1$TERM_TYPE,df$CLASSIFICATION_ID[i],status1$CLASSIFICATION_ID)
  df$noclass_term_5[i] <- vlookup(status1$TERM,df$CLASSIFICATION_ID[i],status1$CLASSIFICATION_ID)
} # add taxon status

if (nrow(status) > nrow(status1)){
  status2 <- status[which(status$TAXON_TERM_ID %!in% status1$TAXON_TERM_ID),] # get status not in status1 by ID
  status2 <- status2[which(!duplicated(status2$CLASSIFICATION_ID)),] # get deduplicated status by ID
  for (i in 1:nrow(df)){
    df$noclass_term_type_6[i] <- vlookup(status2$TERM_TYPE,df$CLASSIFICATION_ID[i],status2$CLASSIFICATION_ID)
    df$noclass_term_6[i] <- vlookup(status2$TERM,df$CLASSIFICATION_ID[i],status2$CLASSIFICATION_ID)
  } # add taxon status
}

if (nrow(status) > (nrow(status1) + nrow(status2))){
  status3 <- status[which(status$TAXON_TERM_ID %!in% status1$TAXON_TERM_ID),] # get status not in status1 or status 2 by ID
  status3 <- status3[which(status3$TAXON_TERM_ID %!in% status2$TAXON_TERM_ID),]
  status3 <- status3[which(!duplicated(status3$CLASSIFICATION_ID)),] # get deduplicated status by ID
  for (i in 1:nrow(df)){
    df$noclass_term_type_7[i] <- vlookup(status3$TERM_TYPE,df$CLASSIFICATION_ID[i],status3$CLASSIFICATION_ID)
    df$noclass_term_7[i] <- vlookup(status3$TERM,df$CLASSIFICATION_ID[i],status3$CLASSIFICATION_ID)
    }
}

#sanity check
if (nrow(status) == sum(!is.na(df$noclass_term_5)) + sum(!is.na(df$noclass_term_6)) + sum(!is.na(df$noclass_term_7))){
  print("yay")
} else {
  print ("problem")
}

remark <- classification[which(classification$TERM_TYPE == "remark"),] # get list of taxon remarks
remark1 <- remark[which(!duplicated(remark$CLASSIFICATION_ID)),] # get deduplicated remark by ID
for (i in 1:nrow(df)){
  df$noclass_term_type_8[i] <- vlookup(remark1$TERM_TYPE,df$CLASSIFICATION_ID[i],remark1$CLASSIFICATION_ID)
  df$noclass_term_8[i] <- vlookup(remark1$TERM,df$CLASSIFICATION_ID[i],remark1$CLASSIFICATION_ID)
} # add taxon remark

if (nrow(remark) > nrow(remark1)){
  remark2 <- remark[which(remark$TAXON_TERM_ID %!in% remark1$TAXON_TERM_ID),] # get remark not in remark1 by ID
  for (i in 1:nrow(df)){
    df$noclass_term_type_9[i] <- vlookup(remark2$TERM_TYPE,df$CLASSIFICATION_ID[i],remark2$CLASSIFICATION_ID)
    df$noclass_term_9[i] <- vlookup(remark2$TERM,df$CLASSIFICATION_ID[i],remark2$CLASSIFICATION_ID)
  } # add taxon remark
}

if (nrow(remark) > (nrow(remark1) + nrow(remark2))){
  remark3 <- remark[which(remark$TAXON_TERM_ID %!in% remark1$TAXON_TERM_ID | remark2$TAXON_TERM_ID),] # get remark not in remark1 or remark 2 by ID
  for (i in 1:nrow(df)){
    df$noclass_term_type_10[i] <- vlookup(remark3$TERM_TYPE,df$CLASSIFICATION_ID[i],remark3$CLASSIFICATION_ID)
    df$noclass_term_10[i] <- vlookup(remark3$TERM,df$CLASSIFICATION_ID[i],remark3$CLASSIFICATION_ID)
  } else {
    print("all remarks added")
  }
}

# sanity check
if (nrow(remark) == sum(!is.na(df$noclass_term_8)) + sum(!is.na(df$noclass_term_9)) + sum(!is.na(df$noclass_term_10))){
  print("yay")
} else {
  print ("problem")
}

prefer <- classification[which(classification$TERM_TYPE == "preferred_name"),] # get list of taxon remarks
prefer1 <- prefer[which(!duplicated(prefer$CLASSIFICATION_ID)),] # get deduplicated remark by ID
for (i in 1:nrow(df)){
  df$noclass_term_type_11[i] <- vlookup(prefer1$TERM_TYPE,df$CLASSIFICATION_ID[i],prefer1$CLASSIFICATION_ID)
  df$noclass_term_11[i] <- vlookup(prefer1$TERM,df$CLASSIFICATION_ID[i],prefer1$CLASSIFICATION_ID)
} # add taxon prefer

if (nrow(prefer) > nrow(prefer1)){
  prefer2 <- prefer[which(prefer$TAXON_TERM_ID %!in% prefer1$TAXON_TERM_ID),] # get prefer not in prefer1 by ID
  for (i in 1:nrow(df)){
    df$noclass_term_type_12[i] <- vlookup(prefer2$TERM_TYPE,df$CLASSIFICATION_ID[i],prefer2$CLASSIFICATION_ID)
    df$noclass_term_12[i] <- vlookup(prefer2$TERM,df$CLASSIFICATION_ID[i],prefer2$CLASSIFICATION_ID)
  } # add taxon prefer
}

if (nrow(prefer) > (nrow(prefer1) + nrow(prefer2))){
  prefer3 <- prefer[which(prefer$TAXON_TERM_ID %!in% prefer1$TAXON_TERM_ID | prefer2$TAXON_TERM_ID),] # get prefer not in prefer1 or prefer 2 by ID
  for (i in 1:nrow(df)){
    df$noclass_term_type_13[i] <- vlookup(prefer3$TERM_TYPE,df$CLASSIFICATION_ID[i],prefer3$CLASSIFICATION_ID)
    df$noclass_term_13[i] <- vlookup(prefer3$TERM,df$CLASSIFICATION_ID[i],prefer3$CLASSIFICATION_ID)
  } else {
    print("all prefers added")
  }
}

# sanity check
if (nrow(prefer) == sum(!is.na(df$noclass_term_11)) + sum(!is.na(df$noclass_term_12)) + sum(!is.na(df$noclass_term_13))){
  print("yay")
} else {
  print ("problem")
}

nomstat <- classification[which(classification$TERM_TYPE == "nomenclatural_code"),] # get list of taxon remarks
nomstat1 <- nomstat[which(!duplicated(nomstat$CLASSIFICATION_ID)),] # get deduplicated remark by ID
for (i in 1:nrow(df)){
  df$noclass_term_type_14[i] <- vlookup(nomstat1$TERM_TYPE,df$CLASSIFICATION_ID[i],nomstat1$CLASSIFICATION_ID)
  df$noclass_term_14[i] <- vlookup(nomstat1$TERM,df$CLASSIFICATION_ID[i],nomstat1$CLASSIFICATION_ID)
} # add taxon prefer

# sanity check
if (nrow(nomstat) == sum(!is.na(df$noclass_term_14))){
  print("yay")
} else {
  print ("problem")
}

# sanity check
p <- (nrow(status) + nrow(prefer) + nrow(source) + nrow(remark) + nrow(nomstat) + nrow(sciname) + nrow(author))

justclass <- classification[which(!is.na(classification$POSITION_IN_CLASSIFICATION)),]

class1 <- justclass[which(justclass$POSITION_IN_CLASSIFICATION == 1),] # get list of taxon remarks
for (i in 1:nrow(df)){
  df$class_term_type_1[i] <- vlookup(class1$TERM_TYPE,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  df$class_term_1[i] <- vlookup(class1$TERM,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  } # first classification term

class1 <- justclass[which(justclass$POSITION_IN_CLASSIFICATION == 2),] # get list of taxon remarks
for (i in 1:nrow(df)){
  df$class_term_type_2[i] <- vlookup(class1$TERM_TYPE,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  df$class_term_2[i] <- vlookup(class1$TERM,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
} # second classification term

class1 <- justclass[which(justclass$POSITION_IN_CLASSIFICATION == 3),] # get list of taxon remarks
for (i in 1:nrow(df)){
  df$class_term_type_3[i] <- vlookup(class1$TERM_TYPE,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  df$class_term_3[i] <- vlookup(class1$TERM,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  } # third classification term

class1 <- justclass[which(justclass$POSITION_IN_CLASSIFICATION == 4),] # get list of taxon remarks
for (i in 1:nrow(df)){
  df$class_term_type_4[i] <- vlookup(class1$TERM_TYPE,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  df$class_term_4[i] <- vlookup(class1$TERM,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
} # fourth classification term

class1 <- justclass[which(justclass$POSITION_IN_CLASSIFICATION == 5),] # get list of taxon remarks
for (i in 1:nrow(df)){
  df$class_term_type_5[i] <- vlookup(class1$TERM_TYPE,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  df$class_term_5[i] <- vlookup(class1$TERM,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  } # fifth classification term

class1 <- justclass[which(justclass$POSITION_IN_CLASSIFICATION == 6),] # get list of taxon remarks
for (i in 1:nrow(df)){
  df$class_term_type_6[i] <- vlookup(class1$TERM_TYPE,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  df$class_term_6[i] <- vlookup(class1$TERM,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  } # sixth classification term

class1 <- justclass[which(justclass$POSITION_IN_CLASSIFICATION == 7),] # get list of taxon remarks
for (i in 1:nrow(df)){
  df$class_term_type_7[i] <- vlookup(class1$TERM_TYPE,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  df$class_term_7[i] <- vlookup(class1$TERM,df$CLASSIFICATION_ID[i],class1$CLASSIFICATION_ID)
  } # seventh classification term

write.csv(df,"~/GitHub/arctos-r/output/class_load.csv", row.names = FALSE)
