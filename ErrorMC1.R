##### ErrorMC1 #####
ErrorMC1= function(
title = "ErrorMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 3, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "Researchers examining ",
quest.txt2 = " found that there were ",
quest.txt3 = ". In fact, there were ",
quest.txt4 = ". What type of error, if any, was committed?", # the above 5 question texts are static text for the full question
studies = c("differences in mean survival times between two groups of mice",
             "differences in mean weight loss between groups of people on several diets",
             "differences in mean scores on a reading comprehension exam given to students who declared separate majors",
             "differences in the proportion of college graduates between groups of people from two separate countries",
             "differences in mean credit card debt between separate classes of students"),  # the above vector of texts are options of different studies for the question
type1 = "significant differences between the groups",
type2 = "no significant differences between the groups",  # the above 2 types are options for statistical conclusion based on the studies
dat.size , # This is the number of values to be randomly generated for the dataset
  digits, # This is the number of decimal places to round off the data
  loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "No Error is not always wrong. It's not a trick answer.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Type 1: Finding significant differences when there are none. Type 2: Finding no significant differences when there are some. No Error: Finding the truth." # This is student feedback, visible after the exam
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
  data1 <- sample(studies, size = 1) # generating random option for the different studies
  data2 <- sample(c(type1, type2), size = 1) # generating random option type of statistical conclusion
  data3 <- sample(c(type1, type2), size = 1)
  corr.ans <- if((data2 == type1) & (data3 == type2)){"Type 1"}
  else{if((data2 == type2) & (data3 == type1)){"Type 2"}
    else{"No Error"}} # This is the correct answer to the question
  decis <- sample(c(1,2), size = 1) # this is vector of random decision number
  ans.txt <- if(which.max(points) == 1)
  {
    if((data2 == type1) & (data3 == type2)){c(" ", sample(c("Type 2", "No Error"), size = 2))}
    else{if((data2 == type2) & (data3 == type1)){c(" ", sample(c("Type 1", "No Error"), size = 2))}
      else{c(" ", sample(c("Type 1", "Type 2"), size = 2))}}
  }
  else{if((which.max(points) == 2) & (decis == 1))
  {
    if((data2 == type1) & (data3 == type2)){c("Type 2", " ", "No Error")}
    else{if((data2 == type2) & (data3 == type1)){c("Type 1", " ", "No Error")}
      else{c("Type 1", " ", "Type 2")}}
  }
    else{if((which.max(points) == 2) & (decis == 2))  
    {
      if((data2 == type1) & (data3 == type2)){c("No Error", " ", "Type 2")}
      else{if((data2 == type2) & (data3 == type1)){c("No Error", " ", "Type 1")}
        else{c("Type 2", " ", "Type 1")}}
    }
      else{if(which.max(points) == 3)
      {
        if((data2 == type1) & (data3 == type2)){c(sample(c("Type 2", "No Error"), size = 2), " ")}
        else{if((data2 == type2) & (data3 == type1)){c(sample(c("Type 1", "No Error"), size = 2), " ")}
          else{c(sample(c("Type 1", "Type 2"), size = 2), " ")}}
      }
      }
    }
  } # These are randomly generated incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, data3,
                                   quest.txt4, collapse = "", sep = ""),
               points.per.q, difficulty, points, hint, feedback) # This is collecting a lot of the above information into a single vector
  options <- c(rep("",6), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
  options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),2] <- content # Indexing and storing all the content
  questions[(1+(8+answers)*i):((8+answers)*(i+1)),3] <- options # Indexing and storing all the content
}
questions <- questions[((9+answers)):((8+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
            row.names=F, col.names=F) # Writing the CSV file
}

ErrorMC1() # creating the csv file