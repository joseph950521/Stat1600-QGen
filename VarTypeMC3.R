##### VarTypeMC3 #####
VarTypeMC3= function(
title = "VarTypeMC3", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 4, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A study's dataset includes the following variable: ",
quest.txt2 = ". What is this variable's type and level of measurement?", # The above 3 questions are static texts for the question
rat.data1 = c("subjects' heights in inches", "subjects' hours spent sleeping per night", "subjects' ages in years",
               "subjects' current number of completed STEM courses", "subjects' years since university admission (2, 3, etc.)"), #randomly generating type of options within the question for ratio data
ord.data1 = c("subjects' heights in ranges (5.0-5.5 ft, 5.5-6 ft, etc)",
               "subjects' weights in ranges (100-125 lbs, 125-150 lbs, etc.)",
               "subjects' tax income brackets", "subjects' military ranks",
               "customers' satisfaction (not satisfied, satisfied, etc.)"), #randomly generating type of options within the question for ordinal data
int.data1 = c("subjects' GPAs in raw points", "subjects' shoe sizes (8, 9, etc.)", "temperatures in celsius",
               "subjects' point grades on statistics tests from different classes (71, 72, etc.)",
               "consumers' review scores of a product (0 to 100)"), #randomly generating type of options within the question for interval data
nom.data1 = c("subjects' racial demographics", "subjects' countries of origin",
               "subjects' marital statuses", "subjects' nationalities",
               "subjects' native language (English, Spanish, etc.)"), #randomly generating type of options within the question for nominal data
dat.size , 
  digits , 
  loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "Try drawing out a tree of all the possible answers. You might be able to eliminate some.", # This is a student hint, visible to them during the exam on e-learning
feedback = "If it's just a label, it's categorical and nominal. If it's a label that can be ordered, it's categorical and ordinal. If it has a number-based measurement scale where just differences are meaningful, it's numeric and interval. If the scale has a true zero and ratios are meaningful, it's numeric and ratio." # This is student feedback, visible after the exam
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
  data1 <- sample(c(rat.data1, ord.data1, int.data1, nom.data1), size = 1) # This is how we're randomly generating the option types from variable and measurement types inside the question
  corr.ans <- if(data1 %in% rat.data1){"Numeric, Ratio"} 
  else{if(data1 %in% ord.data1){"Categorical, Ordinal"}
    else{if(data1 %in% int.data1){"Numeric, Interval"}
      else{"Categorical, Nominal"}}} # This is the correct answer to the question
  ans.bad <- c("Numeric, Ordinal", "Categorical, Interval", "Categorical, Ratio")
  ans.cat <- c("Categorical, Ordinal", "Categorical, Nominal")
  ans.num <- c("Numeric, Interval", "Numeric, Ratio")
  ans.txt <- sample(if(data1 %in% rat.data1){c(ans.bad, ans.cat, "Numeric, Interval")}
                    else{if(data1 %in% ord.data1){c(ans.bad, ans.num, "Categorical, Nominal")}
                      else{if(data1 %in% int.data1){c(ans.bad, ans.cat, "Numeric, Ratio")}
                        else{c(ans.bad, ans.num, "Categorical, Ordinal")}}},
                    size = answers)  # These are randomly generated incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2,
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


VarTypeMC3() # creating the csv file