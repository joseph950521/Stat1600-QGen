##### MeanMC2 #####
library(gridExtra) # the pacakge needed for this particular question to create multiple grid-level plots in a page
MeanMC2= function(
  title = "MeanMC2", # Question-bank title that will be easily viewable in e-learning
  n = 200, # Number of questions to generate
  type = "MC", # The question type, one of many possible types on e-learning
  answers = 2, # Number of answers per MC question
  points.per.q = 4, # Number of points the question is worth (on the test)
  difficulty = 1, # An easily viewable difficulty level on e-learning
  quest.txt1 = "Ian and his friend Neil like playing the video game Mario Kart together. After racing together many times, they hypothesize that Neil averages ",
  quest.txt2 = " race times than Ian. To test their hypothesis, they monitor 50 consecutive races and record the above means and standard deviations of their race times (in minutes). Do these summary statistics help affirm their hypothesis?", # The above 2 question texts are static text for the full question
  Names = c("Ian", "Neil"), #creating a vector of the characters in the question
  digits = 2, # This is the number of decimal places to round off the data
  loc.path = "/Users/josephyang/Desktop/School Stuff/STAT 1600/Course Development/Question Generators/Stat1600-QGen/MeanMC2 images/", # This is the local path used to store any randomly generated image files
  e.path ="Images/", # This is the path on e-learning used to store any above-implemented image files
  hint = "Remember that the mean and SD each measure something very different.", # This is a student hint, visible to them during the exam on e-learning
  feedback = "Means measure center, and SDs measure spread. So, a higher mean = longer race times, a higher SD = less consistent or more variable race times, and vice versa for both mean and SD." # This is student feedback, visible after the exam
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
    data1 <- sample(c("faster", "slower"), size = 1) # This is how we're randomly generating data for this question
    Means <- sample(seq(1, 5, by = .01), size = 2) # This is how we're randomly generating mean data for this question
    SDs <- sample(seq(.5, 2, by = .01), size = 2) # This is how we're randomly generating standard deviation data for this question
    corr.ans <- if((data1 == "faster"))
    {
      if(Means[1] > Means[2]){"Yes"}else{"No"}
    }
    else
    {
      if(data1 == "slower")
      {
        if(Means[2] > Means[1]){"Yes"}else{"No"}
      }
    } # This is the correct answer to the question
    data <- data.frame(Names, Means, SDs, stringsAsFactors = FALSE) # This is how we're randomly generating data frame for this question
    ans.txt <- if(corr.ans == "Yes"){rep("No", 2)}else{rep("Yes", 2)} # These are randomly generated incorrect answers.
    content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, sep = ""),
                 points.per.q, difficulty, paste(e.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
                 points, hint, feedback) # This is collecting a lot of the above information into a single vector
    options <- c(rep("",7), ans.txt, rep("",2)) # This is collecting the incorrect answers above, and indexing them correctly by row
    options[corr.ind] <- corr.ans # This is imputing the correct answer at the appropriate row index
    questions[(1+(9+answers)*i):((9+answers)*(i+1)),1] <- param # This is indexing and storing all the row names
    questions[(1+(9+answers)*i):((9+answers)*(i+1)),2] <- content # Indexing and storing all the content
    questions[(1+(9+answers)*i):((9+answers)*(i+1)),3] <- options # Indexing and storing the answers, both incorrect and correct
    jpeg(filename=paste(loc.path, paste(title, i, sep = "-"), ".jpeg", sep = ""),
         height = 30*nrow(data), width = 55*ncol(data)) # creating the image files in the designated place
    p <- tableGrob(data) #Creating the summary statistics for the graphical item
    grid.arrange(p) #arranging all the summary statistics for the graphical items in one location
    dev.off()
  }
  questions <- questions[((10+answers)):((9+answers)*(n+1)),] # Storing only what's needed for e-learning upload as a CSV file
  write.table(questions, sep=",", file=paste(title, ".csv", sep = ""),
              row.names=F, col.names=F) # Writing the CSV file
}
MeanMC2() # creating the csv file
