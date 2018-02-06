

library(stringr)
# You can get the length of the string with str_length():
str_length("abc")

# You can access individual character using sub_str()
x <- c("abcdef", "ghifjk")
str_sub(x, 3, 3)        # The 3rd letter
str_sub(x, 2, -2)       # The 2nd to 2nd-to-last character
str_sub(x, 3, 3) <- "Z" # Modify the string
x

#To duplicate individual strings, you can use str_dup():
str_dup(x, c(2, 3))

# Whitespace
# str_pad() pads a string to a fixed length by adding extra whitespace 
# on the left, right, or both sides

x <- c("abc", "defghi")
str_pad(x, 10)
str_pad(x, 10, "both")

# The opposite of str_pad() is str_trim(), which removes 
# leading and trailing whitespace

x <- c("  a   ", "b   ",  "   c")
str_trim(x)
str_trim(x, "left")

# str_wrap() to modify existing whitespace in order to wrap a paragraph of 
# text so that the length of each line as a similar as possible

jabberwocky <- str_c(
  "`Twas brillig, and the slithy toves ",
  "did gyre and gimble in the wabe: ",
  "All mimsy were the borogoves, ",
  "and the mome raths outgrabe. "
)
cat(str_wrap(jabberwocky, width = 40))

#Locale sensitive
x <- "I like horses."
str_to_upper(x)
str_to_title(x)
str_to_lower(x)

#string ordering and sorting
x <- c("y", "i", "k")
str_order(x)
str_sort(x)

# Pattern matching
# stringr provides pattern matching functions to 
# detect, locate, extract, match, replace, and split strings

strings <- c(
  "apple", 
  "219 733 8965", 
  "329-293-8753", 
  "Work: 579-499-7527; Home: 543.355.3679")
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

# Which strings contain phone numbers?
str_detect(strings, phone)   # returns logical vector
str_subset(strings, phone)   # returns elements

# How many phone numbers in each string?
str_count(strings, phone)  # counts the number of matches:

# Where in the string is the phone number located?
(loc <- str_locate(strings, phone)) # first position of a pattern and returns a numeric matrix with columns start and end
str_locate_all(strings, phone)      # locates all matches, returning a list of numeric matrices

# What are the phone numbers?
str_extract(strings, phone) # extracts text corresponding to the first match, returning a character vector
str_extract_all(strings, phone) # extracts all matches and returns a list of character vectors

# Pull out the three components of the match
str_match(strings, phone) #returns a character matrix with one column for the complete match and one column for each group
str_match_all(strings, phone) # returns a list of character matrices

str_replace(strings, phone, "XXX-XXX-XXXX") #replaces the first matched pattern and returns a character vector
str_replace_all(strings, phone, "XXX-XXX-XXXX") # replaces all matches

str_split("a-b-c", "-")  # splits a string into a variable number of pieces and returns a list 
str_split_fixed("a-b-c", "-", n = 2) # splits the string into a fixed number of pieces based on a pattern and returns matrix

# Engines
# Fixed matches
a1 <- "\u00e1"
a2 <- "a\u0301"
str_detect(a1, fixed(a2))
str_detect(a1, coll(a2))

# Collation search
# looks for a match to x using human-language collation rules
i <- c("I", "I", "i", "i")
str_subset(i, coll("i", ignore_case = TRUE))
str_subset(i, coll("i", ignore_case = TRUE, locale = "tr"))

# Boundary
# matches boundaries between characters, lines, sentences or words
x <- "This is a sentence."
str_split(x, boundary("word"))
str_count(x, boundary("word"))
str_extract_all(x, boundary("word"))
str_split(x, "") 


# Expressions
see <- function(rx) str_view_all("abc ABC 123\t.!?\\(){}\n", rx)
see
see("a")
see("\\.")
see("\\!")
see("\\?")
see("\\\\")
see("\\(")
see("\\)")
see("\\{")
see( "\\}")
see("\\n")
see("\\t")
see("\\s")
see("\\d")
see("\\w")
see("\\b")
see("[:digit:]")
see("[:alpha:]")
see("[:lower:]")
see("[:upper:]")
see("[:alnum:]")
see("[:punct:]")
see("[:graph:]")
see("[:space:]")
see("[:blank:]")
see(".")


# ALTERNATES 
alt <- function(rx) str_view_all("abcde", rx)
alt
alt("ab|d")    # or
alt("[abe]")   # one of
alt("[^abe]")  # anything but
alt("[a-c]")   # range


# ANCHORS
anchor <- function(rx) str_view_all("aaa", rx)
anchor("^a")   # Start of string
anchor("a$")   # end of string

# LOOK AROUNDS 
look <- function(rx) str_view_all("bacad", rx)
look("a(?=c)")   # followed by
look("a(?!c)")   # not followed by
look("(?<=b)a")  # preceded by
look("(?<!b)a")  # not preceded by

# QUANTIFIERS 
quant <- function(rx) str_view_all(".a.aa.aaa", rx)

quant("a?")       # zero or one 
quant("a*")       # zero or more 
quant("a+")       # one or more 
quant("a{2}")     # exactly n 
quant("a{2,}")    # n or more 
quant("a{2,4}")   # between n and m 

