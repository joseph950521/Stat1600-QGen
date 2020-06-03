##### VarTypeMC2 #####
VarTypeMC2= function(
  title = "VarTypeMC2", # Question-bank title that will be easily viewable in e-learning
  n = 200, # Number of questions to generate
  type = "MC", # The question type, one of many possible types on e-learning
  answers = 2, # Number of answers per MC question
  points.per.q = 4, # Number of points the question is worth (on the test)
  difficulty = 1, # An easily viewable difficulty level on e-learning
  quest.txt1 = "A student decides to take a survey for her class project. She randomly surveys other students and records their ",
  quest.txt2 = ". She decides to use a ",
  quest.txt3 = " to visualize this data. Did she choose a correct graphic?", # The above 3 questions are static texts for the question
  dat.size , # This is the number of values to be randomly generated for the dataset
  digits , # This is the number of decimal places to round off the data
  loc.path, # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "What is the variable's type: categorical or numeric?", # This is a student hint, visible to them during the exam on e-learning
  feedback = "Categorical: Bar, RF table, or Pie. Numeric: Stem & Leaf, RF table, Histogram, Dot, or Box." # This is student feedback, visible after the exam
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
    num.data1 = c("heights in inches", "weights in lbs", "GPAs in raw points",
                  "hours spent sleeping per night", "hours spent studying per day",
                  "vertical leap in inches", "age in years", "calories consumed per day",
                  "current number of completed STEM courses", "years left before their anticipated graduation-date") #randomly generating options within the question for numerical data
    cat.data1 = c("heights in ranges (5.0-5.5 ft, 5.5-6 ft, etc)", "weights in ranges (100-125 lbs, 125-150 lbs, etc.)",
                  "racial demographics", "countries of origin", "emotional states (sad, happy, etc.)",
                  "tax income brackets", "marital statuses", "living arrangements (dorm, off-campus apartment, etc.)",
                  "nationalities", "native language (English, Spanish, etc.)") #randomly generating options within the question for categorical data
    data1 <- sample(c(num.data1, cat.data1), size = 1) # This is how we're randomly generating the option types from variable types inside the question
    num.data2 <- c("relative frequency table", "stem & leaf plot", "histogram",
                   "dotplot", "boxplot") # This is how we're randomly generating the answer options if the variable type is numerical
    cat.data2 <- c("relative frequency table","bar graph", "pie graph") # This is how we're randomly generating the answer options if the variable type is categorical
    data2 <- sample(c(num.data2, cat.data2), size = 1) # This is how we're randomly generating the answer options
    corr.ans <- if(((data1 %in% cat.data1) & (data2 %in% cat.data2)) |
                   ((data1 %in% num.data1) & (data2 %in% num.data2))){"Yes"}
    else{"No"} # This is the correct answer to the question
    ans.txt <- rep(if(corr.ans == "Yes"){"No"}else{"Yes"}, 2) # These are randomly generated incorrect answers
    content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3,
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

VarTypeMC2() # creating the csv file
