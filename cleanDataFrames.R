
#Step A
#Create a function called MyData that pulls census data from a URL and and cleans the rows and columns
MyData <- function()
{
  #Create a variable for the URL link
  URL <- "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  #Use read.csv to read in the CSV file from the URL above
  myData <- read.csv(url(URL))
  #Take the data imported from the CSV file and convert it into a dataframe
  myFrame <- data.frame(myData, stringsAsFactors = FALSE)
  #Delete rows 1 through 8
  myFrame <- myFrame[-(1:8),]
  #Only keep the first 5 columns
  myFrame <- myFrame[,1:5]
  #Rename the rownames as NULL so that they are correctly numbered starting from 1
  rownames(myFrame) <- NULL
  #Delete rows 52 through 58
  myFrame <- myFrame[-52:-58,]
  #Create a variable cnames that represents the column names of myFrame
  cnames <- colnames(myFrame)
  #Set the first column name to StateName
  cnames[1] <- "StateName"
  #Set the second variable name to april10census
  cnames[2]<- "april10census"
  #Set the third variable name to april10base
  cnames[3] <- "april10base"
  #Set the fourth variable name to july10pop
  cnames[4] <- "july10pop"
  #Set the fifth column name to july11pop
  cnames[5] <- "july11pop"
  #Reset the column names using the cnames variable
  colnames(myFrame) <- cnames
  
  #Remove the dots at the beginning of every state within the StateName column
  myFrame$StateName <- gsub("\\.","", myFrame$StateName)
  #Remove the commas within every number for the april10census column
  myFrame$april10census <- gsub(",","",myFrame$april10census)
  #Remove the commas within every number for the april10base column
  myFrame$april10base <- gsub(",","", myFrame$april10base)
  #Remove the commas within every number for the july10pop column
  myFrame$july10pop <- gsub(",","", myFrame$july10pop)
  #Remove the commas within ever number for the july11pop column
  myFrame$july11pop <- gsub(",","", myFrame$july11pop)
  
  #Remove the spaces and covert every observation to a number in the april10census column
  myFrame$april10census <- as.numeric(gsub(" ","",myFrame$april10census))
  #Remove the spaces and covert every observation to a number in the april10base column
  myFrame$april10base <- as.numeric(gsub(" ","", myFrame$april10base))
  #Remove the spaces and covert every observation to a number in the july10pop column
  myFrame$july10pop <- as.numeric(gsub(" ","", myFrame$july10pop))
  #Remove the spaces and covert every observation to a number in the july11pop column
  myFrame$july11pop <- as.numeric(gsub(" ","", myFrame$july11pop))
  
  
  #Return the clean dataframe back into the MyData variable
  return(myFrame)
  
}
#Run the myData function and store it in dfStates
dfStates <- MyData()

#Create a new variable called arrests that contains the USArrests data
arrests <- USArrests
