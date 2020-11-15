##### RFTableMC1 #####
library(gridExtra) # the pacakge needed for this particular question to create multiple grid-level plots in a page
RFTableMC1= function(
title = "RFTableMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "For her class project in statistics, a student researcher decides to survey other students and track the number of ramen noodle eaters she can find in various majors. She decides to summarize her findings in the depicted (incomplete) relative frequency table. What is the relative frequency of ramen noodle eaters who major in ",
quest.txt2 = "?", # The above 2 question texts are static text for the full question
majors = c("Physics", "Statistics", "Speech Pathology", "Psychology", "Music", "Philosophy",
            "Mathematics", "Fine Arts", "Chemistry", "Sociology", "Education", "Business",
            "Accounting", "Geology", "Occupational Therapy", "Nursing", "Medicine",
            "Electrical Engineering", "Mechanical Engineering", "Civil Engineering",
            "English", "Spanish", "French", "Arabic", "Economics", "Applied Mathematics"), # This is a vector of major for the question
dat.size = 5:15, # This is the number of values to be randomly generated for the dataset
digits = 2, # This is the number of decimal places to round off the data
loc.path = "/Users/josephyang/Desktop/School Stuff/STAT 1600/Course Development/Question Generators/Stat1600-QGen/RFTableMC1 images/", # This is the local path used to store any randomly generated image files
e.path = "Images/", # This is the path on e-learning used to store any above-implemented image files
hint = "Read the Data Presentation chapter of your coursepack.", # This is a student hint, visible to them during the exam on e-learning
feedback = "For the specified category, calculate frequency over total and multiply by 100%." # This is student feedback, visible after the exam
)
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty", "Image",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 7 + which.max(points) # This is the row index of the correct answer
  dat.size1 <- sample(dat.size, size = 1) # randomly generating sample size for the question
  Majors <- sample(majors, size = dat.size1) # randomly generating sample size for the major option for the question
  data1 <- sample(Majors, size = 1) # randomly selecting a single major option for the question
  RamenEaters <- sample(seq(100, 400, by = 3), size = dat.size1) # randomly generating number of ramen eaters for each of the major option
  RF.calc <- round(RamenEaters/sum(RamenEaters)*100, digits = digits) # equation for calculating relative frequency
  RF <- paste(RF.calc, "%", sep = "")
  data.calc <- data.frame(Majors, RF.calc)
  data <- data.frame(Majors, RamenEaters, RF, stringsAsFactors = FALSE) # This is how we're randomly generating data frame for this question
  corr.ans <- RF[Majors == data1] # This is the correct answer to the question
  data[Majors == data1, 3] <- NA
  data <- rbind(data, c("Total", sum(RamenEaters), "100%")) # creating a final row of total values beneath the data frame
  ans.txt <- c(sample(c(paste(seq((RF.calc[Majors == data1] + 1), 40, by = 10^-digits), "%", sep = ""),
                        paste(seq(0, (RF.calc[Majors == data1] - 1), by = 10^-digits), "%", sep = "")),
                      size = answers-1),
               sample(c("None of These", "All of These"), size = 1)) # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
               points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
               points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
  jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
       height = 25*nrow(data), width = 120*ncol(data)) # creating the image files in the designated place
  p <- tableGrob(data) #Creating the summary statistics for the graphical item
  grid.arrange(p) #arranging all the summary statistics for the graphical items in one location
  dev.off()
}
questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

RFTableMC1() # creating the csv file