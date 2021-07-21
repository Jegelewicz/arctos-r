# this script will transform horizontal parts with muliple attributes to a vertical list

# load libraries
library(readxl)

#define functions
# define function VLOOKUP (x = return value dataframe and column,
# y = lookup value dataframe and column, z = lookup dataframe and column
# x and z should be from the same dataframe)
vlookup <- function(x,y,z){
  x[match(y,z)]
}

df <- read_excel("~/GitHub/arctos-r/input/parts.xlsx") # read in horizontal part file

colnames(df) <- tolower(colnames(df)) # lower case column names

# # Get max number of parts in horizontal file
# parts <- 0 # set count to zero
# for(i in 1:ncol(df)) { # for every column in the file
#   name <- colnames(df)[i] # review each column name
#   if(grepl('part_name', name, perl = TRUE) == TRUE) { # if column name contains "part_name" then
#     parts <- parts + 1 # add one to part count
#    }
# }
#
# # create part bulkload GUID Prefix file
# # pb <-  subset(df, select=c("guid_prefix", "cat_num")) # retain GUID Prefix and catalog number columns
# # pb[ ,c("status","part_name","condition","disposition","lot_count","remarks","container_barcode","parent_part_barcode","parent_part_name")] <- NA # add part bulkload column headers (excluding attributes)
# 
# # Loop through the numbers of parts to create separate dataframes
# for(i in 1:parts){ 
#   # create part bulkload GUID Prefix file
#   pb <-  subset(df, select=c("guid_prefix", "cat_num")) # retain GUID Prefix and catalog number columns
#   pb[ ,c("status","part_name","condition","disposition","lot_count","remarks","container_barcode","parent_part_barcode","parent_part_name")] <- NA # add part bulkload column headers (excluding attributes)
#   # assign(paste('pb',i,sep=''),pb) # create a dataframe for part number
#   partname <- paste('part_name_', i, sep="")
#   condition <- paste('condition_', i, sep="")
#   disposition <- paste('disposition_', i, sep="")
#   lotcount <- paste('lot_count_', i, sep="")
#   remark <- paste('remarks_', i, sep="")
#   # barcode <- paste('container_barcode_', i, sep="")
#   # parentbar <- paste('parent_part_barcode_', i, sep="")
#   # parentname <- paste('parent_part_name_', i, sep="")
#   for(j in 1:nrow(pb)){
#     pb$part_name[j] <- vlookup(df[[partname]],pb$cat_num[j],df$cat_num) # look up part name by cat num
#     pb$condition[j] <- vlookup(df[[condition]],pb$cat_num[j],df$cat_num) # look up part condition by cat num
#     pb$disposition[j] <- vlookup(df[[disposition]],pb$cat_num[j],df$cat_num) # look up part disposition by cat num
#     pb$lot_count[j] <- vlookup(df[[lotcount]],pb$cat_num[j],df$cat_num) # look up part lot count by cat num
#     pb$remarks[j] <- vlookup(df[[remark]],pb$cat_num[j],df$cat_num) # look up part remarks by cat num
#     # pb$part_barcode[j] <- vlookup(df[[barcode]],pb$cat_num[j],df$cat_num) # look up part barcode by cat num
#     # pb$parent_barcode[j] <- vlookup(df[[parentbar]],pb$cat_num[j],df$cat_num) # look up part parent barcode by cat num
#     # pb$parent_name[j] <- vlookup(df[[parentname]],pb$cat_num[j],df$cat_num) # look up part parent name by cat num
#   }
#   assign(paste('pb',i,sep=''),pb) # put result in a separate dataframe
# }

notnamelist <- c()
for(x in 1:ncol(df)) { # for every column in the file
  notname <- colnames(df)[x] # review each column name
  if(grepl("other_id", notname, perl = TRUE) == TRUE) { # if column name contains "part_name" then
    notnamelist <- c(notnamelist,notname)
  }
}

df <- df[ , -which(names(df) %in% c("z","u"))]

parts <- 0 # set count to zero
for(i in 1:ncol(df)) { # for every column in the file
  name <- colnames(df)[i] # review each column name
  if(grepl('part_name', name, perl = TRUE) == TRUE) { # if column name contains "part_name" then
    parts <- parts + 1 # add one to part count
  }
}
for(i in 1:parts){ 
colnamelist <- c("guid_prefix","cat_num")
  for(j in 1:ncol(df)) { # for every column in the file
    name <- colnames(df)[j] # review each column name
    if(grepl(i, name, perl = TRUE) == TRUE) { # if column name contains "part_name" then
      colnamelist <- c(colnamelist,name)
    }
    pb <- subset(df, select=c(colnamelist))
  }
assign(paste('pb',i,sep=''),pb) # put result in a separate dataframe
}
