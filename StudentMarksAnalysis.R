#Read data from student CSV file
df1 = read.csv("StudentScoreData.csv")

#(nrow(df1[df1$Result == "N.C.L",]))
#(nrow(df1[df1$Result == "FAIL",]))
#(nrow(df1[df1$Result == "DISTI",]))

#Display sample data 
#head(df1)

#Display all data in new window
View(df1)

#Remove students who are absent for EXAM i.e. Result == N.C.L
df2=df1[df1$Result != "N.C.L",]

#Select Only College Name, Result and Score column data
df3 =df2[c(2:4)] 

#Convert Score from Character to Numeric
df4 = transform(df3, Score=as.numeric(Score))

#select unique List Of Colleges 
LOC = unique(df4$CollegeName)

#Create a data frame(SDAF-> Student Analytics Data Frame) to store our calculations 
SADF = data.frame(numberOfStudents = integer(), 
                  numberOfDistinctions = integer(), 
                  numberOfFirstClass = integer(),
                  numberOfSecondClass = integer(), 
                  numberOfFailed = integer(), 
                  meanResult = numeric(), 
                  sdResult = numeric(), 
                  varResult = numeric(),
                  nameOfCollege = character())

for(cname in LOC){#extract individual college Name from the list
  
  temp = df4[df4$CollegeName == cname,]#extract student data of specific college and store in temp data frame
  
  #calculate the no. of students, no. of students with Distinction, first class, second class and Fail
  no_of_students = nrow(temp)
  no_of_distinctions = nrow(temp[temp$Result == "DISTI",])
  no_of_firstclass = nrow(temp[temp$Result == "1ST C",])
  no_of_secondclass = nrow(temp[temp$Result == "2ND C",])
  no_of_failure = nrow(temp[temp$Result == "FAIL",])
  
  #calculate mean, SD, Variance
  mean_result = round(mean(temp$Score), 2)
  sd_result = round(sd(temp$Score), 2)
  var_result = round(var(temp$Score), 2)
  
  #create temporary data frame of specific college
  sdaf_temp = data.frame(numberOfStudents = no_of_students, 
                         numberOfDistinctions = no_of_distinctions, 
                         numberOfFirstClass = no_of_firstclass,
                         numberOfSecondClass = no_of_secondclass, 
                         numberOfFailed = no_of_failure, 
                         meanResult = mean_result, 
                         sdResult = sd_result,
                         varResult = var_result, 
                         nameOfCollege = cname)
  
  #copy above college data to SDAF -> Student Analytics Data Frame
  SADF = rbind(SADF, sdaf_temp)
  
}



#------------------------------------------------------------------------------
#Graphical Representation
#------------------------------------------------------------------------------
library(tidyverse)

#get colleges with number of students more than 10
nSADF = subset(SADF, numberOfStudents > 10 )


#--------------------------------------------------
#1)Colleges with max students
#--------------------------------------------------
ggplot(nSADF, aes(x=numberOfStudents, y=reorder(nameOfCollege, numberOfStudents))) + 
  geom_bar(stat = "identity") 



#--------------------------------------------------
#2)Colleges with highest Distinction Ratio
#--------------------------------------------------
#add new column fo nSDAF called distRatio
nSADF$distRatio=nSADF$numberOfDistinctions / nSADF$numberOfStudents

ggplot(nSADF, aes(x=distRatio, y=reorder(nameOfCollege, distRatio))) + 
  geom_bar(stat = "identity")



#--------------------------------------------------
#3)No of students of college in top 10 rankings
#--------------------------------------------------
data1=df2[c(1:4)]
#convert string to numeric
data2=transform(data1, Score=as.numeric(Score))
#sort score by descending order
data2=data2[order(data2$Score, decreasing = TRUE), ]
#get only top 10 scores
data3=data2[1:10,]
#Create a data frame(temp1) to store aggregate students of college
temp1 = data.frame(numberOfStudents = integer(),nameOfCollege = character())
#extract unique college Names
LOC = unique(data3$CollegeName)

for(cname in LOC){
  temp = data3[data3$CollegeName == cname,]
  #number of students of specific college
  no_of_students = nrow(temp)
  #create temporary data frame of specific college
  data_temp = data.frame(numberOfStudents = no_of_students,nameOfCollege = cname)
  #copy above college data to temp1
  temp1 = rbind(temp1, data_temp)
}

ggplot(temp1, aes(x=numberOfStudents, y=reorder(nameOfCollege, numberOfStudents))) + 
  geom_bar(stat="identity")




#--------------------------------------------------
#4)colleges with students who scored top 10% 
#--------------------------------------------------
data1=df2[c(1:4)]
#convert string to numeric
data2=transform(data1, Score=as.numeric(Score))
#sort score by descending order
data3=data2[order(data2$Score, decreasing = TRUE), ]
n=nrow(data3)
top_10p=ceiling(n*0.1)
data4=data3[1:top_10p,]

temp1 = data.frame(numberOfStudents = integer(), 
                   nameOfCollege = character())
LOC = unique(data4$CollegeName)
for(cname in LOC){
  temp = data4[data4$CollegeName == cname,]
  no_of_students = nrow(temp)
  #create temporary data frame of specific college
  data_temp = data.frame(numberOfStudents = no_of_students, 
                         nameOfCollege = cname)
  #copy above college data to SDAF -> Student Analytics Data Frame
  temp1 = rbind(temp1, data_temp)
}
ggplot(temp1, aes(x=numberOfStudents, y=reorder(nameOfCollege, numberOfStudents))) + 
  geom_bar(stat="identity")



#--------------------------------------------------
#5)Passing Ratio College List
#--------------------------------------------------
nSADF$passingRatio = (nSADF$numberOfStudents-nSADF$numberOfFailed)/nSADF$numberOfStudents
ggplot(nSADF, aes(x=passingRatio, y=reorder(nameOfCollege, passingRatio))) +
  geom_bar(stat="identity")



#--------------------------------------------------
#6)Avg score of students of the College
#--------------------------------------------------
ggplot(nSADF, aes(x=meanResult, y=reorder(nameOfCollege, meanResult)))+
  geom_bar(stat="identity")



#--------------------------------------------------
#7)SD of Students score of college
#--------------------------------------------------
ggplot(nSADF, aes(x=sdResult, y=reorder(nameOfCollege, sdResult)))+
  geom_bar(stat="identity")


#--------------------------------------------------
#8)no. of Students not appeared for the exam w.r.t college
#--------------------------------------------------

#Select Only College Name, Result and Score column data
df7 =df1[c(2:4)] 


#Create a data frame(SDAF-> Student Analytics Data Frame) to store our calculations 
SADF = data.frame(numberOfStudents = integer(), 
                  nameOfCollege = character(),
                  numberOfAbsenties = integer())

for(cname in LOC){#extract individual college Name from the list
  
  temp = df7[df7$CollegeName == cname,]#extract student data of specific college and store in temp data frame
  
  #calculate the no. of students, no. of students with Distinction, first class, second class and Fail
  no_of_students = nrow(temp)
  no_of_absenties = nrow(temp[temp$Result == "N.C.L",])
  
  #create temporary data frame of specific college
  sdaf_temp = data.frame(numberOfStudents = no_of_students, 
                
                         numberOfAbsenties = no_of_absenties, 
                          
                         nameOfCollege = cname)
  
  #copy above college data to SDAF -> Student Analytics Data Frame
  SADF = rbind(SADF, sdaf_temp)
  
}

ggplot(SADF, aes(x=numberOfAbsenties, y=reorder(nameOfCollege, numberOfAbsenties)))+
  geom_bar(stat="identity")

