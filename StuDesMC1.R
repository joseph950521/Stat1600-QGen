##### StuDesMC1 #####
StuDesMC1= function(
title = "StuDesMC1", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A friend of yours wonders, '",
quest.txt2 = "' You respond by asking, '",
quest.txt3 = "' ",
quest.txt4 = " just ...", # The above four texts are static texts for the question
wonder1 = "Which diet, Atkins or Zone, is best?",
wonder2 = "Which exercise program, Zumba or Insanity, is most effective?",
wonder3 = "Which drugs are most damaging to youths?",
wonder4 = "Which type of credit card is the most generous?",
wonder5 = "Which language is the most difficult to learn?", # the above five texts are wondering options to include in the question
response1 = "Which diet results in the greatest reduction in BMI over the first three months?",
response2 = "Which program results in the largest muscle mass increase over the first three months?",
response3 = "Which drugs (marijuana, alcohol, etc.) are most associated with academic drop-out rates in youths?",
response4 = "Which brand of credit card (Visa, Mastercard, etc.) averages the lowest interest rates?",
response5 = "Which language (Chinese, Arabic, etc.) takes the longest average learning-time to reach fluency as rated by a native speaker?", # the above five texts are response options to include in the question
dat.size , # This is the number of values to be randomly generated for the dataset
  digits , # This is the number of decimal places to round off the data
  loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "The important concepts here are from the Knowledge and Data chapter of your coursepack.", # This is a student hint, visible to them during the exam on e-learning
feedback = "Coming up with a basic idea using vague, often normative, language is conceptualizing a problem. Attaching a measurable outcome to the question is operationalizing the problem." # This is student feedback, visible after the exam
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
  data1 <- sample(c(wonder1, wonder2, wonder3, wonder4, wonder5), size = 1) # randomly generating the options of wonder in the question
  data2 <- if(data1 == wonder1){response1}
  else{if(data1 == wonder2){response2}
    else{if(data1 == wonder3){response3}
      else{if(data1 == wonder4){response4}
        else{response5}
      }
    }
  } # generating the exact response depending on the wonder option
  data3 <- sample(c("You", "Your friend"), size = 1)
  corr.ans <- if(data3 == "You"){"... Operationalized the Problem"}
  else{"... Conceptualized the Problem"} # This is the correct answer to the question
  ans.txt <- if(data3 == "You"){c("... Conceptualized the Problem",
                                  sample(c("... Designed the Study", "... Committed the Lack of Evidence Fallacy",
                                           "... Committed the Anecdotal Evidence Fallacy",
                                           "... Committed the Correlation Equals Causation Fallacy",
                                           "... Collected the Data", "... Analyzed the Data",
                                           "... Drew Conclusions from the Study", "... Disseminated Results",
                                           "... Committed a Type 1 Error", "... Committed a Type 2 Error"),
                                         size = answers - 1))
  }
  else{c(sample(c("... Designed the Study", "... Committed the Lack of Evidence Fallacy",
                  "... Committed the Anecdotal Evidence Fallacy",
                  "... Committed the Correlation Equals Causation Fallacy",
                  "... Collected the Data", "... Analyzed the Data",
                  "... Drew Conclusions from the Study", "... Disseminated Results",
                  "... Committed a Type 1 Error", "... Committed a Type 2 Error"),
                size = answers - 1),
         "... Operationalized the Problem")
  } # These are the randomly positioned incorrect answers
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, data2, quest.txt3, data3,
                                   quest.txt4, collapse = "", sep = ""),
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

StuDesMC1() # creating the csv file