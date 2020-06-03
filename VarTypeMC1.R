##### VarTypeMC1 #####
VarTypeMC1= function(
title = "VarTypeMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A student wants to visualize data that is ",
quest.txt2 = ". What kind of graph should she select for this purpose?", # The above 2 questions are static texts for the question
dat.size ,
  digits , 
  loc.path, # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "Focus on the variable type: categorical vs. numeric.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Categorical: Bar or Pie. Numeric: Stem & Leaf, Histogram, Dot, or Box."
) # This is student feedback, visible after the exam
{
param <- c("NewQuestion","ID","Title","QuestionText","Points","Difficulty",
           rep("Option", answers),"Hint","Feedback") # These are setting row names for the CSV file
questions <- data.frame() # This opens a data frame to store the randomly generated questions below
for(i in 1:n)
{
  ID <- paste(title, i, sep = "-") # The ID of the specific question within the bank, title + question number in the loop
  points <- sample(c(rep(0,answers-1),100),replace=F) # The proportion of points assigned to each possible answer, 1 if correct or 0 if incorrect
  corr.ind <- 6 + which.max(points) # This is the row index of the correct answer
  data <- sample(c("categorical", "numerical"), size = 1) # This is how we're randomly generating the variable types inside the question
  cat.ans <- c("Bar Graph", "Pie Graph") # This is how we're randomly generating the answer options if the variable type is categorical
  cat.supp <- c("None of These", "All of These") # This is how we're randomly generating other options
  num.ans <- c("Stem & Leaf Plot", "Histogram", "Dot Plot", "Box Plot") # This is how we're randomly generating the answer options if the variable type is numerical
  corr.ans <- ifelse(data == "categorical", sample(cat.ans, size = 1), sample(num.ans, size = 1)) # This is the correct answer to the question
  ans.txt <- if(data == "categorical"){sample(c(num.ans, "Ordinal", "Nominal"), size = answers)} 
  else{c(cat.ans, sample(c("Interval", "Ratio"), size = answers - 3), 
         sample(cat.supp, size = 1))} # These are randomly generated incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data, quest.txt2,
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

VarTypeMC1() # creating the csv file