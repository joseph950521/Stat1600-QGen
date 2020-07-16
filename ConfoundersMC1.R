##### ConfoundersMC1 #####
ConfoundersMC1= function(
title = "ConfoundersMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "Some student research assistants are helping study ",
quest.txt2 = ". The study primarily involves three variables. First, the ",
quest.txt3 = " sometimes leads to a change in a second variable, the ",
quest.txt4 = ". However, sometimes a third variable, the ",
quest.txt5 = " (which is associated with the ",
quest.txt6 = " and causes changes in the ",
quest.txt7 = ") interferes with the result. In statistics, what do we call the ",
quest.txt8 = "?", # The above 5 question texts are static text for the full question
example1 = c("flowers", "flowers' petal length", "flowers' reproductive success", "flowers' height"),
example2 = c("other students' academic success", "amount of time students spend studying", "students' GPAs", "students' work schedules"),
example3 = c("beehives", "number of bees in the hive", "survival of the beehive through winter", "availability of nectar in summer"),
example4 = c("the student debt problem", "amount of debt accrued while in school", "amount of debt remaining ten years later", "choice of major"),
example5 = c("mental health", "amount of time a patient spends outside", "state of a patients' depression", "patients' occupation"),
example6 = c("sports health and football", "length of kickoff plays", "number of concussions during kickoff plays", "speed of the players"),
example7 = c("consumers in the music industry", "location of consumers", "consumers' preferred genre of music", "consumers' education level"),
example8 = c("the craft brewing industry", "size of beer-brewing companies", "growth of these companies for the year", "average price of their products"), # The above 8 example texts are the different options to choose from to use in the question
dat.size , 
  digits , # This is the number of decimal places to round off the data
  loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = ".", # This is a student hint, visible to them during the exam on e-learning
feedback = "." # This is student feedback, visible after the exam
)
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 6 + which.max(points) # This is the row index of the correct answer
  decis1 <- sample(1:8, size = 1) # these are random decision numbers
  data <- get(paste("example", decis1, sep = "")) # randomly generating example options based on the decision numbers
  decis2 <- sample(2:4, size = 1) # these are the decision numbers for the four option in the answer section
  corr.ans <- if(decis2 == 2){"The Probable Cause"}
  else{if(decis2 == 3){"The Outcome/Effect"}
    else{if(decis2 == 4){"The Confounder"}}} # This is the correct answer to the question
  ans.txt <- if(decis2 == 2){c(sample(c("The Outcome/Effect", "The Confounder",
                                        "The Standard Deviation", "The Fallacy"),
                                      size = answers-1),
                               sample(c("All of the Above", "None of the Above"),
                                      size = 1))}
  else{if(decis2 == 3){c(sample(c("The Confounder", "The Standard Deviation",
                                  "The Probable Cause", "The Fallacy"),
                                size = answers-1),
                         sample(c("All of the Above", "None of the Above"),
                                size = 1))}
    else{if(decis2 == 4){c(sample(c("The Outcome/Effect", "The Standard Deviation",
                                    "The Probable Cause", "The Fallacy"),
                                  size = answers-1),
                           sample(c("All of the Above", "None of the Above"),
                                  size = 1))}}} # These are randomly generated incorrect answers.
  content <- c(type, ID, ID, paste(quest.txt1, data[1], quest.txt2, data[2], quest.txt3, data[3],
                                   quest.txt4, data[4], quest.txt5, data[2], quest.txt6, data[3],
                                   quest.txt7, data[decis2], quest.txt8,
                                   collapse = "", sep = ""),
               points.per.q, difficulty, points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",6), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
}
questions <- questions[((9+answers)):((8+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

ConfoundersMC1() # creating the csv file