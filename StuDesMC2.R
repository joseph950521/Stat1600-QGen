##### StuDesMC2 #####
StuDesMC2 = function(
title = "StuDesMC2", # Question-bank title that will be easily viewable in e-learning
n = 200, # Number of questions to generate
type = "MC", # The question type, one of many possible types on e-learning
answers = 5, # Number of answers per MC question
points.per.q = 4, # Number of points the question is worth (on the test)
difficulty = 1, # An easily viewable difficulty level on e-learning
quest.txt1 = "A team of researchers ",
quest.txt2 = " What type of study is this?", # The above two texts are static texts for the question
study1 = "randomly separates their study's participants into two groups, giving one group a placebo and the other a new treatment to be tested. As the treatment is very experimental, both participants and researchers know whether a specific participant is receiving the new treatment or not.",
study2 = "randomly separates their study's participants into two groups, giving one group a placebo and the other a new treatment to be tested. As the treatment is not experimental, both participants and researchers do not know whether a specific participant is receiving the new treatment or not.",
study3 = "randomly separates their study's participants into two groups, giving one group a placebo and the other a new treatment to be tested. As the treatment is somewhat experimental, participants do not know whether they receive the placebo but researchers do know whether a specific participant receives the placebo.",
study4 = "studies a population of people who lost weight, asking each person in a sample from this population whether they were on a diet and, if so, which diet was it?",
study5 = "studies whether hormone therapy affects menstrual cycles in women. All women in the sample under study were monitored for 2 months without hormone therapy, monitored for 2 more months while being given hormone therapy A, and monitored for 2 more months (for 6 months total) while being given hormone therapy B. Each woman thus acted as her own control. At the end of the 6 months, all participants' menstrual cycles were compared.",
study6 = "studies the effect of a new pesticide on two randomly selected samples from a species of crop-destroying beetles. One group of beetles receives a standard pesticide, and the other receives the new pesticide. Neither the beetles nor the researchers know which pesticide is which, and survival rates for each group are compared throughout administration of several doses.",
study7 = "studies the effect of a new medicine meant to treat the common cold. Two randomly selected groups of people are chosen for the study; one group receives a standard cold medication while the other gets the new medication. Due to an error in study design, the researchers are aware of which treatment each participant is receiving, although the participants themselves are unaware whether they received the standard or new medications.",
study8 = "conducts a study of a new medicine meant to treat the common cold. Two randomly selected groups of people are chosen for the study; one group receives a standard cold medication while the other gets the new medication. Due to an error in study design, both the study's participants and the researchers are aware of which treatment each participant is receiving.",
study9 = "conducts a preliminary study of a population of people who are suffering from Alzheimer's disease. The researchers are interested in learning whether potassium intake plays a role in contracting Alzheimer's. Study participants are asked, when possible, about their long-term dietary habits; of particular interest is whether participants regularly consumed foods or supplements that contain potassium such as bananas.",
study10 = "conducts a study of a typical course of treatment for recurring pneumonia that involves a sequence of three medications A, B, and C. The sequence is of interest, so subjects are given the sequence of medications A, B, then C on the first occurence of pneumonia, then subjects are given the sequence C, B, then A on the second occurence, etc. All sequences are given to each subject, so that each subject acts as their own control while comparing the effects of the different sequences on pneumonia.", # the above 10 texts are study options to include in the question
dat.size , # This is the number of values to be randomly generated for the dataset
  digits, # This is the number of decimal places to round off the data
  loc.path , # This is the local path used to store any randomly generated image files
  e.path , # This is the path on e-learning used to store any above-implemented image files
  hint = "The important concepts here are from the Study Designs chapter of your coursepack.", # This is a student hint, visible to them during the exam on e-learning
feedback = "RCT: randomization, control group. Single-Blinding: researchers aware, subjects unaware. Double-Blinding: both researchers and patients unaware. Non-Blnded: Neither patients nor researchers aware. Case-Control: Looks back from outcome to treatment, no randomization. Case-Crossover: Participants act as own control group." # This is student feedback, visible after the exam
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
  data1 <- sample(c(study1, study2, study3, study4, study5,
                    study6, study7, study8, study9), size = 1) # randomly generating the options of study topic in the question
  corr.ans <- if(data1 == study1 | data1 == study8){"Non-Blinded Randomized Controlled Trial"}
  else{if(data1 == study2 | data1 == study6){"Double-Blinded Randomized Controlled Trial"}
    else{if(data1 == study3 | data1 == study7){"Single-Blinded Randomized Controlled Trial"}
      else{if(data1 == study4 | data1 == study9){"Case-Control Study"}
        else{if(data1 == study5 | data1 == study10){"Case-Crossover Study"}}}}} # This is the correct answer to the question based on the options
  ans.txt <- if(corr.ans == "Non-Blinded Randomized Controlled Trial")
  {sample(c("Double-Blinded Randomized Controlled Trial", "Single-Blinded Randomized Controlled Trial",
            "Case-Control Study", "Case-Crossover Study", "Type 1 Study", "Type 2 Study", "Standard Deviation Study"),
          size = answers)}
  else{if(corr.ans == "Double-Blinded Randomized Controlled Trial")
  {sample(c("Non-Blinded Randomized Controlled Trial", "Single-Blinded Randomized Controlled Trial",
            "Case-Control Study", "Case-Crossover Study", "Type 1 Study", "Type 2 Study", "Standard Deviation Study"),
          size = answers)}
    else{if(corr.ans == "Single-Blinded Randomized Controlled Trial")
    {sample(c("Non-Blinded Randomized Controlled Trial", "Double-Blinded Randomized Controlled Trial",
              "Case-Control Study", "Case-Crossover Study", "Type 1 Study", "Type 2 Study", "Standard Deviation Study"),
            size = answers)}
      else{if(corr.ans == "Case-Control Study")
      {sample(c("Non-Blinded Randomized Controlled Trial", "Single-Blinded Randomized Controlled Trial",
                "Double-Blinded Randomized Controlled Trial", "Case-Crossover Study", "Type 1 Study", "Type 2 Study", "Standard Deviation Study"),
              size = answers)}
        else{if(corr.ans == "Case-Crossover Study")
        {sample(c("Non-Blinded Randomized Controlled Trial", "Single-Blinded Randomized Controlled Trial",
                  "Case-Control Study", "Double-Blinded Randomized Controlled Trial", "Type 1 Study", "Type 2 Study", "Standard Deviation Study"),
                size = answers)}}}}} # These are the randomly positioned incorrect answers based on the correct answer option
  content <- c(type, ID, ID, paste(quest.txt1, data1, quest.txt2, collapse = "", sep = ""),
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

StuDesMC2() # creating the csv file
