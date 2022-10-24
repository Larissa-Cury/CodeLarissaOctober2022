###########################################################################################################
#############################This code belongs to Larissa da Silva Cury (UFRGS)#################################3
###########################################################################################################

### ANT_ANALYSIS_CODE

###install.packages("ddlyr","tidyverse")

library(tidyverse)
library(writexl) ### if you want to export the file to Excel 

### set work directory
## if you don't know your path, run getwd()

setwd("YOUR PATH HERE")

### Read PciBex's function

read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

### Read in results file

results <- read.pcibex("THE NAME OF YOUR FILE HERE.csv")

############################################### create new collumn "Randowm" with 1:n(row)#####################################
 results1 <- results %>% 
                       mutate(Random = as.numeric(seq(1:n()))
   )

###########################run Jeremy's function#################################################################

add_blocks <- function (df,id) {
  PRACTICE_TO_FIRST <- max(df$Random[df$Label=="instructions_E"])
  FIRST_TO_SECOND <- max(df$Random[df$Label=="instructions_F"])
  SECOND_TO_THIRD <- max(df$Random[df$Label=="instructions_G"])
  df$block <- 0
  df$block[df$Random>PRACTICE_TO_FIRST] <- 1
  df$block[df$Random>FIRST_TO_SECOND] <- 2
  df$block[df$Random>SECOND_TO_THIRD] <- 3
  return(df)
}

####################apply the function on the data_frame (create new df called "results_blocks)#####################################

results_blocks <- results1 %>%
                  group_by(id) %>%
                  group_modify(add_blocks)


######################################## clean table ########################################

data_results <- results_blocks %>%
  filter(Parameter %in% c("Key","PressedKey")) %>%   ####filter answers S, K, None
  select(id,Label,imagens,Value,Parameter,ReactionTime,block) %>% #### filter relevant columns
  group_by(id) %>%  ####group by id
  mutate(Answer = case_when(is.na(Value) ~ "0", ####create a new column called "answers" with S, K and 0 (none)
                            Value == "S" ~ "S",
                            Value == "K" ~ "K")) %>%
  mutate(Result = case_when(imagens == "cong-left.png" & Answer == "S" ~ "correct",
                            imagens == "neutral-left.png" & Answer == "S" ~ "correct",
                            imagens == "incong-left.png" & Answer == "S" ~ "correct",
                            imagens == "cong-right.png" & Answer == "K" ~ "correct",
                            imagens == "neutral-right.png" & Answer == "K" ~ "correct",
                            imagens == "incong-right.png" & Answer == "K" ~ "correct",
                            TRUE ~ "incorrect")) %>%
  select(-Value) %>%  ####erase the old collumn
  select(id,Label,imagens,Answer,Result,ReactionTime,block) ####


###################################export to Excel############################

write_xlsx(data_results, "YOUR PATH HERE/THE NAME OF YOUR FILE HERE.xlsx")

#Now we can start statistics!!!  
  
label <- data_results %>% 
             count(Label,id,block)
  