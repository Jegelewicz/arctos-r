library(readxl)

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

# function: remove '\xa0' chars
phrase_clean <- function(x) gsub("[\xA0]", "", x)

# function: replace double spaces with single spaces
space_clean <- function(x) gsub("  ", " ", x)
