library(readxl)
library(plyr)

# define function: is not in
'%!in%' <- function(x,y)!('%in%'(x,y))

# define function: right
right = function (string, char) {
  substr(string,(unlist(lapply(gregexpr(pattern = char, string), min)) + 1),nchar(string))
}

# define function: left
left = function (string,char) {
  substr(string,1,unlist(lapply(gregexpr(pattern = char, string), min)))
}

# define function VLOOKUP (x = return value dataframe and column,
# y = lookup value dataframe and column, z = lookup dataframe and column
# x and z should be from the same dataframe)
vlookup <- function(x,y,z){
  x[match(y,z)]
}

# define function: name length
name_length <- function(x) ifelse(!is.na(x), length(unlist(strsplit(x, ' '))), 0)
