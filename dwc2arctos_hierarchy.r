# this script will convert a DarwinCore taxonomy file to an Arctos hierarchical taxonomy upload file

# add libraries
library(readxl)
library(data.table)

# Replace "~/GitHub/arctos_r/input/filename.xlsx" with the file name and directory where the file to be converted is found
df <- read_excel("~/GitHub/arctos-r/input/filename.xlsx", 
                                  col_types = c("text", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "text", "text", "text", "text", "numeric", 
                                                "numeric", "numeric", "numeric", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "text", "text", "text", "text", 
                                                "text", "numeric", "text", "numeric", 
                                                "text", "text", "text", "text", "text")) #read in DwC file to transform

names(df)[names(df) == 'canonicalName'] <- 'scientific_name' #change canonicalName to scientific_name

names(df)[names(df) == 'taxonRank'] <- 'name_rank' #change taxonRank to taxon_rank

# get parent name if not supplied
for(i in 1:nrow(df)){
  df$parentNameUsage[i] <- ifelse(!is.na(df$parentNameUsage[i]), df$parentNameUsage[i], # use parentNameUsage is available
                                  ifelse(is.na(df$name_rank[i]), NA, # if both parentNameUsage and taxon_rank are blank, insert NA
                                         ifelse(df$name_rank[i] == "subspecies", df$specificEpithet[i],   
                                                ifelse(df$name_rank[i] == "species", ifelse(is.na(df$subgenus[i]), df$genus[i], df$subgenus[i]) ,# if name rank is species and there is no subgenus, insert genus, otherwise insert genus
                                                       ifelse(df$name_rank[i] == "subgenus", df$genus[i], # if name rank is species, insert genus
                                                              ifelse(df$name_rank[i] == "genus", df$family[i], #if name rank is genus, insert family
                                                                     ifelse(df$name_rank[i] == "family", df$order[i], #if name rank is family, insert order
                                                                            NA)) # otherwise NA
                                                       )
                                                )
                                         )
                                         
                                  )
  )
}

names(df)[names(df) == 'parentNameUsage'] <- 'parent_name'

# replace "user" with the Arctos username of the person who will upload the file
df$username <- "user" # fill in username

# replace "filename"hierarchy" with the name of the hierarchy to be uploaded
df$hierarchy_name <- "hierarchy" # fill in hierarchy name

names(df)[names(df) == 'scientificNameAuthorship'] <- 'noclass_term_1' # change column with scientifcNameAuthorship to no_class_term_1
df$noclass_term_type_1 <- ifelse(!is.na(df$noclass_term_1), 'author_text', NA) #set fourth non classification term to "author_text" if no_class_term_1 is not NA

names(df)[names(df) == 'nomenclaturalCode'] <- 'noclass_term_2' # change column with nomenclaturalCode to no_class_term_2
df$noclass_term_2 <- ifelse(is.na(df$noclass_term_2), 'ICZN', df$noclass_term_2) # set all nomenclatural code terms
df$noclass_term_type_2 <- ifelse(!is.na(df$noclass_term_2), 'nomenclatural_code', NA) # set second non classification term type to nomenclatural_code

df$noclass_term_type_3 <- 'source_authority' # set third non classification term to source_authority
df$noclass_term_3 <- 'Terrestrial Parasite Tracker Thematic Collection Network' # set third non classification term to Terrestrial Parasite Tracker Thematic Collection Network

names(df)[names(df) == 'taxonRemarks'] <- 'noclass_term_4' # change column with taxonRemark to no_class_term_4
df$noclass_term_type_4 <- ifelse(!is.na(df$noclass_term_4), 'taxon_remark', NA) #set fourth non classification term to taxon_remark if no_class_term_4 is not NA

names(df)[names(df) == 'taxonomicStatus'] <- 'noclass_term_5' # change column with  to no_class_term_5
df$noclass_term_type_5 <- ifelse(!is.na(df$noclass_term_5), 'taxon_status', NA) #set fourth non classification term to taxon_status if no_class_term_5 is not NA
df$noclass_term_5 <- ifelse(df$noclass_term_5 == 'accepted', 'valid', df$noclass_term_5) # adjust to Arctos code table terms

names(df)[names(df) == 'acceptedNameUsage'] <- 'noclass_term_6' # change accepteNameUsage to preferred_name
df$noclass_term_type_6 <- ifelse(!is.na(df$noclass_term_6), 'preferred_name', NA) #set fourth non classification term to preferred_name if no_class_term_6 is not NA

# dealing with duplicates
df$reason <- c(ifelse(duplicated(df$scientific_name, fromLast = TRUE)  | duplicated(df$scientific_name),
                      "dupe", NA)) # Flag internal dupes
dupes <- df[which(grepl('dupe',df$reason) == TRUE), ] # get all duplicates
dupes <- df[which(duplicated(df$scientificName, fromLast = TRUE)  | duplicated(df$scientificName)),] # only keep those with duplicated scientific names

# order column names
# df[,c(1,2,3,4)]. Note the first comma means keep all the rows, and the 1,2,3,4 refers to the columns.
columns <- df[, colnames(df)[c(grepl("^noclass_term", colnames(df)))]]
df1 <- subset(df, select = grep("^noclass_term", names(df)))
Arctos_upload <- df[,c("username",
                       "hierarchy_name",
                       "scientific_name",
                       "name_rank",
                       "parent_name",
                       "noclass_term_type_1",
                       "noclass_term_1",
                       "noclass_term_type_2",
                       "noclass_term_2",
                       "noclass_term_type_3",
                       "noclass_term_3",
                       "noclass_term_type_4",
                       "noclass_term_4",
                       "noclass_term_type_5",
                       "noclass_term_5",
                       "noclass_term_type_6",
                       "noclass_term_6"
)
]

# replace "~/GitHub/arctos_r/output/Arctos_upload.csv" with the directrory and filename to save the csv to
write.csv(Arctos_upload,"~/GitHub/arctos_r/output/Arctos_upload.csv", row.names = FALSE) # write out csv for upload
