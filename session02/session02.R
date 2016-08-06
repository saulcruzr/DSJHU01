##Author: Saul Cruz Robles
##session 02 challenge

##1) Create a single function to load the dataset of complete cases given the folder location: eg. load_complete('specdata') 
##function name:load_complete
##parameter: @directory : specifies the folder path and folder name where multiple .csv files are. i.e. '"~/Desktop/specdata/' or 'specdata' if it's in the same folder where the script resides

load_complete <-function(directory){
        
        ##creates a list of all files in the specified @directory
        filesList <- list.files(path.expand(directory), ignore.case=TRUE)
        nFiles<-length(filesList)
        
        df <- data.frame(Date=as.Date(as.character(), format = "%y-%m-%d"),
                         sulfate=numeric(), 
                         nitrate=character(), 
                         ID=integer()) 
        
        for(i in 1:nFiles)
        {
                
                filepath<-paste(directory,"/",filesList[i],sep = '')
                dsfile<- read.csv(filepath,stringsAsFactors = FALSE)
                ##filter out rows that are incomplete (having NA in either column)
                dsfiltered<-dsfile[complete.cases(dsfile),]
                ##append two data frames: 00i.csv file and accumulated df 
                df<-rbind(df,dsfiltered)
        }
        #returns a data frame with all files completed cases data
        df
}

##2) What are the dimensions (number of rows and columns) of the dataset. 
##111802 rows and 4 columns
result<-load_complete("specdata")
dim(result)


##3) Specify the column name and the data type for each column of the dataset. 
sapply(result,class)
##question is not clear on whether we want to set column names and data types or get them, the above gets both column names and data types
##column names were extracted from the files and the data types was defined in the first function load_complete()

##4) What is the minimum and maximum of all the numeric columns of the dataset. 
apply(result,2,min) ##the apply is working only on the numeric/int values, date field is character
apply(result,2,max) 

##5) What is the date range of the data included. (earliest and latest day that can be found) in the dataset 

##2000-02-09
min(as.POSIXct(result$Date, format="%Y-%m-%d"))
##2010-05-14
max(as.POSIXct(result$Date, format="%Y-%m-%d"))


##6) Get the daily mean for the sulfate polutant levels in the dataset. 
##3.194204
aggregate(result$sulfate,by=list(result$Date),mean)

##7) Get the mean nitrate levels for each monitor in the dataset.
aggregate(result$nitrate,by=list(result$ID),mean)

##Using iris dataset 
##8) Load the iris dataset 
data("iris")

##9) What are the dimensions of the iris dataset? 
#150 rows and 5 columns
dim(iris)

##10) What are the column name and the data type for each column in the iris dataset? 
sapply(iris,class)

##11) What is the minimum and maximum of all the numeric columns in the iris dataset? 
apply(iris,2,min) ##the apply is working only on the numeric/int values, date field is character
apply(iris,2,max) 

##12) What are the different categories of species that exist in the iris dataset?
#setosa, veriscolor, virginica
unique(iris$Species)

##13) What is the mean sepal length for the species versicolor in the iris dataset?
meanresult<-aggregate(iris$Sepal.Length,by=list(iris$Species=='versicolor'),mean)
meanresult[meanresult$Group.1==TRUE,]$x

##14) Obtain a vector with the means of the sepal lenght, sepal width, petal length and petal width across all species from the iris dataset.

apply(iris[,-5],2,mean) 


##15) Obtain the mean petal length for each of the species.

aggregate(iris$Petal.Length,by=list(iris$Species),mean)