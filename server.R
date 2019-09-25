server <- function(input, output, session) {
  # toggle state for the prepare data button (become active only when either a dataset or the toy dataset are uploaded)
  observe({
    shinyjs::toggleState("load", 
                         !is.null(input$datafile) | input$checkbox == T)
  })
  # toggle state for the update button (become active only when a D-score is selected)
  observe({
    shinyjs::toggleState("update", input$sel_d != 0)
  })
  # toggle state for the Download button (become active only when the update buttton has been clicked at least once)
  observe({
    shinyjs::toggleState("downloadData", input$update > 0)
  })
  # create reactive object where data, results, and options can be stored
  values <- reactiveValues()
  # check whether the toy dataset has been selected 
  dataentry <- observe({
    if (input$checkbox == T){
      datasetInput <- reactive({
        # define the wd for the example dataset 
        # AND HAS TO BE CHANGED ACCORDINGLY
        dataset <- read.csv("~/GitHub/DscoreApp/raceAPP.csv")
      })
    } else {
      # if the toy dataset has not been found --> import and store users' dataset
      # Import data
      datasetInput <- reactive({
        infile <- input$datafile
        if (is.null(infile)) {
          # User has not uploaded a file yet
          return(NULL)
        }
        isolate({
          input$load
          dataset <- read.csv(infile$datapath)
        })
        dataset
      })
    }
    # store the data in the reactive object
    observe({
      values$dataset <- data.frame(datasetInput())
    })
    # store the blcok labels that are in the dataframe
    observe({
      values$dataset$block_label <- isolate(values$dataset$block)
    })
  })
  # Select variable MappingA practice block
  output$label_mapA_practice <- renderUI({
    # save the unique blcoks labels as they are in the dataframe
    labels.options <- unique(values$dataset$block)
    selectInput("mapA_practice", h5("e.g. practiceWhiteGood"), 
                choices = labels.options,
                # display the first label as default
                labels.options[1])
  })  
  # select variable MappingA test block
  output$label_mapA_test <- renderUI({
    # save the unique blcoks labels as they are in the dataframe
    labels.options <- unique(values$dataset$block)
    selectInput("mapA_test", h5("e.g. testWhiteGood"), 
                choices = labels.options,
                # display the second label as default
                labels.options[2] )
  })
  # select variable MappingB practice block
  output$label_mapB_practice <- renderUI({
    # save the unique blcoks labels as they are in the dataframe
    labels.options <- unique(values$dataset$block)
    selectInput("mapB_practice", h5("e.g. practiceWhiteBad"), 
                choices = labels.options,
                # display the third label as default
                labels.options[3] )
  })  
  # select variable MappingA test block
  output$label_mapB_test <- renderUI({
    # save the unique blcoks labels as they are in the dataframe
    labels.options <- unique(values$dataset$block)
    selectInput("mapB_test", h5("e.g. testWhiteBad"), 
                choices = labels.options,
                # display the fourth label as default
                labels.options[4])
  })
  
  ### labels check 
  labels_check<- observeEvent(
    input$load, 
    {
      # check whether there are more blocks labels than expected 
      if (length(unique(values$dataset$block_label)) > 4){
        alert <- "There are more blocks than expected. Remove the extra blocks and restart the app." 
        values$alert <- "restart" # create and save an alert
      }
      # if the number of blocks labels is correct, check whether users tried to select the same label for two blocks
      # if they did, an alert is created and saved
      else if(input$mapA_practice == input$mapA_test){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else if(input$mapA_practice == input$mapB_practice){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else if(input$mapA_practice == input$mapB_test){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else if(input$mapB_practice == input$mapB_test){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else if(input$mapB_practice == input$mapA_test){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else if(input$mapB_test == input$mapA_test){
        alert <- "check your labels and restart the app!"
        values$alert <- "restart"
      }
      else{
        return()
      }
      # if at least one the previous conditions is true, an alert is displayed 
      shinyjs::alert(alert)
    }
  )
  # Prepare the dataframe for the D-score computation
  newentry <- observeEvent(
    input$load,
    {
      # rename blocks labels in MappingA and MappingB to create the IAT conditions variable
      values$dataset$Condition <- as.character(values$dataset$block)
      values$dataset$Condition <- with(values$dataset,
                                       ifelse(block == input$mapA_practice |
                                                block == input$mapA_test, 
                                              "MappingA",
                                              ifelse(
                                                block == input$mapB_practice |
                                                  block == input$mapB_test, 
                                                "MappingB",
                                                "error")))
      # Create order of presentation variable and save it in a dataframe
      values$condition_order <- with(values$dataset,
                                     aggregate(Condition, 
                                               by = list(participant), 
                                               FUN = unique))
      colnames(values$condition_order) <- c("participant", "order")
      # create the variable of the order of blocks presentation
      values$condition_order$cond_ord <- paste(values$condition_order$order[,1],
                                               values$condition_order$order[,2],
                                               sep = "_") 
      # select just the participant and condition order variables
      values$condition_order <- values$condition_order[, c("participant", 
                                                           "cond_ord")]
      # rename the order so that is consistent with the "MappingA" and "MappingB"
      values$condition_order$cond_ord <- with(values$condition_order,
                                              ifelse(
                                                cond_ord == "MappingA_MappingB",
                                                "MappingA_First",
                                                "MappingB_First"))
      # create the legend for MappingA
      values$condition_order$legendMappingA <- paste(input$mapA_practice,
                                                     "and",input$mapA_test, 
                                                     sep = "_")
      # create the legend for MappingB
      values$condition_order$legendMappingB <- paste(input$mapB_practice,
                                                     "and",input$mapB_test, 
                                                     sep = "_")
      
      # rename the block level in practice and test block
      # save the original starting blocks labels in a new variable
      values$dataset$blockR <- as.character(values$dataset$block)
      # crate the block_pool variable (just practice vs test)
      # needed for the computation of the pooled sd
      values$dataset$block_pool <- with(values$dataset,
                                        ifelse(block == input$mapA_practice |
                                                 block == input$mapB_practice, 
                                               "practice",
                                               ifelse(
                                                 block == input$mapA_test |
                                                   block == input$mapB_test, 
                                                 "test",
                                                 "error"
                                               )))
      # create the block variable (practice_MappingA, practice_MappingB,
      # test_MappingA, test_MappingB)
      values$dataset$blockR <- with(values$dataset,
                                    paste(block_pool, Condition, 
                                          sep = "_"))
      
      # both data with and without built in have the same column 'latency' containing the RTs
      # in the built-in correction case, the latency has to be corrected with the inflation for the responses beforehand
      # create a variable identifying slow trials ( > 10,000 ms)
      values$dataset$slow <- ifelse(values$dataset$latency > 10000, 
                                    "no", "yes")
      # create a variable identifying fast responses to be eliminated according to the D-score selected (< 400 ms)       
      values$dataset$fast400 <- with(values$dataset,
                                     ifelse(latency < 400, 
                                            "no", "yes"))
      # create a variable identifying fast response (< 300 ms) for the elimination of the fast participants
      values$dataset$fast300 <- ifelse(values$dataset$latency < 300, 
                                       "no", "yes")
      
      # number of slow responses for each participant
      values$num_slow <- data.frame(with(values$dataset, 
                                         table(slow, participant)))
      values$num_slow <- values$num_slow[values$num_slow$slow %in% "yes", ]
      # number of trials actually performed before any deletion 
      values$num_trial <- data.frame(with(values$dataset, 
                                          table(participant)))
      colnames(values$num_trial) <- c("participant", "n_trial")
      # merge the number of slow trials with the number of trails
      values$num_slow <- merge(values$num_slow, values$num_trial, 
                               by = "participant")
      # compute the difference between the number of trials actually peformed and the number of slow trials
      values$num_slow$slow10000 <- with(values$num_slow, 
                                        n_trial - Freq)
      values$num_slow <- values$num_slow[, c("participant", "n_trial",
                                             "slow10000")]
      # number of fast reasponses (< 300) for each participant
      values$num_fast300 <- data.frame(with(values$dataset,
                                            table(fast300, participant)))
      values$num_fast300 <- values$num_fast300[values$num_fast300$fast300 %in% 
                                                 "no", c("participant", "Freq")]
      colnames(values$num_fast300) <- c("participant", "num.300")
      
      # number of fast responses (< 400) for each participant
      values$num_fast400 <- data.frame(with(values$dataset,
                                            table(fast400, participant)))
      values$num_fast400 <- values$num_fast400[values$num_fast400$fast400 %in% 
                                                 "no", c("participant", "Freq")]
      colnames(values$num_fast400) <- c("participant", "num.400")
      
      # compute the percentage of fast responses
      # needed for deciding whether to eliminate participants or not
      values$dataset$participant <- as.character(values$dataset$participant)
      # pnumber of trials < 300 ms for each participant
      values$sbj_300 <- data.frame(with(values$dataset, 
                                        table(latency < 300, participant)))
      # select only the lines that evaluated in TRUE
      values$sbj_300 <- values$sbj_300[values$sbj_300$Var1 %in% "TRUE", c(2,3)]
      values$sbj_300$participant <- as.character(values$sbj_300$participant)
      # create the decision variable for the fast responses participants deletion (if it evaluates in TRUE --> "out) 
      for(i in 1:length(unique(values$dataset$participant))){
        values$sbj_300$out_fast <- ifelse(values$sbj_300$Freq > 
                                            (table(values$dataset$participant)[i])*0.10, 
                                          "out", "keep")
        
      }
      colnames(values$sbj_300)[2] <- "n_trial300"
      values$sbj_300$participant <- as.character(values$sbj_300$participant)
      # merge dataset to values$sbj_300 to create the filter variable
      values$dataset <- merge(values$dataset, values$sbj_300, 
                              by = "participant")
      # create variable for the output of total number of slow responses
      values$slow <- values$dataset[values$dataset$slow %in% "no", ]
      # create a varible for telling whether data are ready
      values$ready <- values$dataset[values$dataset$slow %in% "yes", ]
      # compute proportion of correct responses for each participant in each condition
      values$correct_response <- with(values$dataset,
                                      aggregate(correct, 
                                                by = list(Condition, participant),
                                                FUN = mean))
      colnames(values$correct_response) <- c("Condition", "participant", 
                                             "prop_correct_cond")
      # reahspe in wide format 
      values$correct_response_wide <- reshape(values$correct_response, 
                                              idvar = "participant",
                                              timevar = "Condition", 
                                              direction = "wide")
      # calculate the proportion of error responses (error_cond)
      values$correct_response$error_cond <- with(values$correct_response,
                                                 1 - prop_correct_cond)
      # merge original dataframe with the proportion of error responses to create the filter variable
      values$dataset <- merge(values$dataset, values$correct_response,
                              by = c("participant", "Condition"))
      # compute proportion of correct responses for each participant in each block
      values$accuracy_block <- with(values$dataset,
                                    aggregate(correct, 
                                              by = list(participant, blockR), 
                                              FUN = mean) )
      colnames(values$accuracy_block) <- c("participant", "block", 
                                           "p_correct_block")
      # reshape in wide format
      values$accuracy_block_wide <- reshape(values$accuracy_block,
                                            idvar = "participant", 
                                            timevar = "block",
                                            direction = "wide")
      # compute proportion of correct responses for each participant in each block_pool (practice vs test)
      values$accuracy_block_pool <- with(values$dataset,
                                         aggregate(correct, 
                                                   by = list(participant, 
                                                             block_pool), 
                                                   FUN = mean))
      colnames(values$accuracy_block_pool) <- c("participant", "block_pool", 
                                                "p_correct_bpool")
      # reshape the dataframe
      values$accuracy_block_pool_wide <- reshape(values$accuracy_block_pool, 
                                                 idvar = "participant",
                                                 timevar = "block_pool", 
                                                 direction = "wide")
      # compute overall proportion of correct responses for each participant
      values$accuracy_tot <- with(values$dataset,
                                  aggregate(correct, by = list(participant), 
                                            FUN = mean))
      colnames(values$accuracy_tot) <- c("participant", "p_correct_tot")
      # merge accuracy_block and accuracy_block_pool
      values$accuracy <- merge(values$accuracy_block_wide,
                               values$accuracy_block_pool_wide, 
                               by = "participant")
      # merge overall accuracy with correct_response_wide (proportion of correct responses in each condition)
      values$accuracy <- merge(values$accuracy, 
                               values$correct_response_wide, 
                               by = "participant")
      # merge accuracy with accuracy accuracy_tot
      values$accuracy <- merge(values$accuracy, 
                               values$accuracy_tot, 
                               by = "participant")
      ## descriptive information on participants' time performance
      # merge number of fast trials (both < 300 ms and < 400 ms)
      values$fast_sbj <- merge(values$num_fast300, values$num_fast400,
                               by = "participant")
      # merge the number of slow responses with the number of fast responses
      values$time <- merge(values$num_slow, values$fast_sbj,
                           by = "participant")
      # overall RTs average for each participant 
      values$subject_mean <- with(values$dataset,
                                  aggregate(latency,
                                            by = list(participant), 
                                            FUN = mean))
      
      colnames(values$subject_mean) <- c("participant", "mean.tot")
      # merge the time dataset (containing the information on fast and slow responses) with the overall average response time
      values$time <- merge(values$time, values$subject_mean,
                           by = "participant")
      values$time <- merge(values$time, values$accuracy, 
                           by = "participant")
      # take out slow responses 
      values$dataset <- values$dataset[values$dataset$slow %in% "yes", ]
    })
  
  # tell the user whether the data are ready to use
  output$data_ready <- renderUI({
    
    loading <- ifelse(is.null(values$ready), ("Waiting for data"), 
                      "Data are ready!") 
    helpText(h3(loading))
  })
  
  # prevent buttons ####
  # toggle state for the update button (become active only when a D-score is selected and there's something in the ready object)
  observe({
    shinyjs::toggleState("update", input$sel_d != 0 && !(is.null(values$ready)))
  })
  # toggle state for the Select D drop-down menu (become active only when there's something in the ready object)
  observe({
    shinyjs::toggleState("sel_d", !(is.null(values$ready)))
  })
  # toggle state for the Accuracy deletion option (become active only when a D-score is selected)
  observe({
    shinyjs::toggleState("accuracy_del", input$sel_d != 0 )
  })
  # toggle state for the Accuracy deletion percentage option (become active only when a D-score is selected
  observe({
    shinyjs::toggleState("accuracy_del", input$sel_d != 0 )
  })
  # toggle state for the Fast participants deletion option (become active only when a D-score is selected
  observe({
    shinyjs::toggleState("sbjFast_del", input$sel_d != 0 )
  })
  
  ### calculate the  D-score ####
  cleandata <- observeEvent(
    input$update, 
    {
      # Compute the D-score according to the specific algorithm selected by the users
      if(input$sel_d == 1){
        # d1: built in, no lower tail treatment
        values$out_400 <- "Not expected for this D"
        values$d1 <- values$dataset
        # create the variable latency_cor for the computatiopn of the D-score
        values$d1$latency_cor <- values$d1$latency
        values$data <- values$d1
        values$d_select <- 1
      } 
      else if(input$sel_d == 2){
        # d2: built in, lower tail treatment 400ms
        values$out_400 <- sum(values$dataset$fast400 == "no", na.rm = T)
        values$d2 <- values$dataset[values$dataset$fast400 %in% "yes", ]
        # create the variable latency_cor for the computatiopn of the D-score
        values$d2$latency_cor <- values$d2$latency
        values$data <- values$d2
        values$d_select <- 2
      } 
      else if(input$sel_d == 3){
        values$out_400 <- "Not expected for this D"
        # d3: no built in, no lower tail treatment, error = mean + 2*sd
        values$d3 <- values$dataset
        # Compute the mean on the correct responses for the error correction
        values$correct_time_d3 <- values$d3[which(values$d3$correct == 1), ]
        values$mean_correct_d3 <- with(values$correct_time_d3,
                                       aggregate(latency, 
                                                 by = list(blockR, participant),
                                                 FUN = mean))
        colnames(values$mean_correct_d3) <- c("blockR", "participant", "mean")
        # merge original data with mean on correct responses
        values$d3 <- merge(values$d3, values$mean_correct_d3,
                           by = c("participant", "blockR"))
        
        # Compute the sd on the correct responses for the error correction
        values$sd_correct_d3 <- with(values$correct_time_d3,
                                     aggregate(latency, 
                                               by = list(blockR, participant),
                                               FUN = sd))
        colnames(values$sd_correct_d3) <- c("blockR", "participant", "sd_block")
        # merge original data with correct sd
        values$d3 <- merge(values$d3, values$sd_correct_d3,
                           by = c("participant", "blockR"))
        # compute the penalty mean + 2*sd
        values$d3$sd_penalty <- with(values$d3,
                                     mean +  (2 * sd_block))
        # if the respone is incorrect --> penalty, otherwise latency
        values$d3$latency_cor <- with(values$d3,
                                      ifelse(correct == 0, 
                                             sd_penalty, latency))
        values$data <- values$d3
        values$d_select <- 3
      } 
      else if(input$sel_d == 4) {
        values$out_400 <- "Not expected for this D"
        # d4: no built in, no lower tail treatment, error = mean + 600
        values$d4 <- values$dataset 
        # Compute the mean on the correct responses for the error correction
        values$correct_time_d4 <- values$d4[which(values$d4$correct == 1), ]
        values$mean_correct_d4 <- with(values$correct_time_d4,
                                       aggregate(latency, 
                                                 by = list(blockR, participant),
                                                 FUN = mean))
        colnames(values$mean_correct_d4) <- c("blockR", "participant", "mean")
        # merge original data with correct mean
        values$d4 <- merge(values$d4, values$mean_correct_d4,
                           by = c("participant", "blockR"))
        # compute the peanlty mean + 600
        values$d4$penalty <- with(values$d4,
                                  mean + 600)
        # if teh response is incorrect --> penalty, otherwise latency
        values$d4$latency_cor <- with(values$d4, 
                                      ifelse(correct == 0, penalty, latency))
        values$data <- values$d4
        values$d_select <- 4
        
      } 
      else if(input$sel_d == 5){
        #d5: no built in, lower tail treatment, error = mean + 2*sd
        values$out_400 <- sum(values$dataset$fast400 == "no", na.rm = T)
        values$d5 <- values$dataset[values$dataset$fast400 %in% "yes", ]
        
        # Compute the mean on the correct responses for the error correction
        values$correct_time_d5 <- values$d5[which(values$d5$correct == 1), ]
        values$mean_correct_d5 <- with(values$correct_time_d5,
                                       aggregate(latency, by = list(blockR, participant),
                                                 FUN = mean))
        colnames(values$mean_correct_d5) <- c("blockR", "participant", "mean")
        # merge original data to correct mean
        values$d5 <- merge(values$d5, values$mean_correct_d5,
                           by = c("participant", "blockR"))
        # Compute the sd on the correct responses for the error correction
        values$sd_correct_d5 <- with(values$correct_time_d5,
                                     aggregate(latency, by = list(blockR, participant),
                                               FUN = sd))
        colnames(values$sd_correct_d5) <- c("blockR", "participant", "sd_block")
        # merge origianl data to sd correct
        values$d5 <- merge(values$d5, values$sd_correct_d5,
                           by = c("participant", "blockR"))
        # compute penalty mean + 2*sd
        values$d5$sd_penalty <- with(values$d5,
                                     mean + (2 * sd_block))
        values$d5$latency_cor <- with(values$d5,
                                      ifelse(correct == 0, sd_penalty, latency))
        values$data <- values$d5
        values$d_select <- 5
      } 
      else if (input$sel_d == 6){
        # d6: no builtin, lower tail treatment, error = mean + 600
        values$out_400 <- sum(values$dataset$fast400 == "no", na.rm = T)
        values$d6 <- values$dataset[values$dataset$fast400 %in% "yes", ]
        
        # Compute the mean on the correct responses for the error correction
        values$correct_time_d6 <- values$d6[which(values$d6$correct == 1), ]
        values$mean_correct_d6 <- with(values$correct_time_d6,
                                       aggregate(latency, by = list(blockR, participant),
                                                 FUN = mean))
        colnames(values$mean_correct_d6) <- c("blockR", "participant", "mean")
        # merge original data with correct mean
        values$d6 <- merge(values$d6, values$mean_correct_d6,
                           by = c("participant", "blockR"))
        # compute penalty mean + 600
        values$d6$penalty <- with(values$d6,
                                  mean + 600)
        values$d6$latency_cor <- with(values$d6, 
                                      ifelse(correct == 0, penalty, latency))
        values$data <- values$d6
        values$d_select <- 6
      }
      
      # Actually compute the Dscore
      # compute the vraiance on the blocik pool (practice vs test)
      values$variance <- with(values$data,
                              aggregate(latency_cor, 
                                        by = list(participant, block_pool),
                                        FUN = var))
      colnames(values$variance) <- c("participant", "block_pool", "variance")
      # compute the mean for each subject in each block
      values$sbj_mean <- with(values$data,
                              aggregate(latency_cor, 
                                        by = list(participant, blockR),
                                        FUN = mean))
      colnames(values$sbj_mean) <- c("participant", "blockR", "mean")
      # create a variable indicating just whether the block was a practice or a test block, so that this dataframe can be merged with the datfarme containing the variance
      values$sbj_mean$block_pool <- values$sbj_mean$block
      values$sbj_mean$block_pool  <- gsub(".MappingA", '', 
                                          values$sbj_mean$block_pool)
      values$sbj_mean$block_pool  <- gsub(".MappingB", '', values$sbj_mean$block_pool )
      values$sbj_data <- merge(values$variance, 
                               values$sbj_mean, 
                               by = c("participant","block_pool"))
      # reshape in wide format
      values$sbj_data_wide <- reshape(values$sbj_data, 
                                      idvar = "participant", 
                                      timevar = "blockR",
                                      direction = "wide")
      # select only useful variables 
      values$sbj_data_wide <- values$sbj_data_wide[, 
                                                   c("participant", 
                                                     "block_pool.practice_MappingA", 
                                                     "variance.practice_MappingA",   
                                                     "mean.practice_MappingA",
                                                     "mean.test_MappingA", 
                                                     "block_pool.test_MappingB",
                                                     "variance.test_MappingB",
                                                     "mean.test_MappingB",
                                                     "mean.practice_MappingB")]
      # rename the columns
      colnames(values$sbj_data_wide) <- c("participant", 
                                          "block_pool_practice_MappingA", 
                                          "variance_practice",   
                                          "mean_practice_MappingA", 
                                          "mean_test_MappingA", 
                                          "block_pool_test_MappingB",
                                          "variance_test", 
                                          "mean_test_MappingB", 
                                          "mean_practice_MappingB")
      # compute the difference in the average response time for the practice bloks of the two mappings
      values$sbj_data_wide$diff_practice <- with(values$sbj_data_wide,
                                                 mean_practice_MappingB - 
                                                   mean_practice_MappingA)
      # compute the difference in the average response time for the tests bloks of the two mappings
      values$sbj_data_wide$diff_test <- with(values$sbj_data_wide,
                                             mean_test_MappingB - 
                                               mean_test_MappingA)
      # compute the D-score for the practice blocks 
      values$sbj_data_wide$d_practice <- with(values$sbj_data_wide,
                                              diff_practice/sqrt(variance_practice))
      # compute the D-score for the test blocks 
      values$sbj_data_wide$d_test <- with(values$sbj_data_wide,
                                          diff_test/sqrt(variance_test))
      # compute the D-score as the mean between the practice and test D-score
      values$sbj_data_wide$dscore <- with(values$sbj_data_wide,
                                          (rowSums(
                                            values$sbj_data_wide[,c("d_practice", 
                                                                    "d_test")]))/2)
      # select only useful columns
      values$dframe <- values$sbj_data_wide[, c("participant", "d_practice", 
                                                "d_test", "dscore")]
      # merge the dataset containing the D-score with the dataset containing the details on participants performance
      values$descript_data <- merge(values$time, values$dframe,
                                    by = "participant")
      # merge the descript_data dataset with the dataset containing the order of presentation of the blocks
      values$descript_data <- merge(values$descript_data, 
                                    values$condition_order)
      # specificy which D-score was compute by pasting the number of the D-score to the d_practice, d_test and d_score variables
      colnames(values$descript_data)[16:18] <- paste(colnames(
        values$descript_data)[16:18], input$sel_d, sep = "_")
      # compute the accuracy based on the percentage enetered by the users
      values$dataset$test_acc <- with(values$dataset,
                                      ifelse(values$dataset$error_cond > 
                                               input$perc_error/100,
                                             "out", "keep"))
      # create a dataframe containing the IDs of the partciipants to eliminate based on the accuracy deletion
      values$sbj_accuracy <- values$dataset[values$dataset$test_acc %in% "out", ]
      # create a dataframe containing the IDs of the participants to eliminate based on fast responses
      values$sbj_time <- values$dataset[values$dataset$out_fast %in% "out", ]
      # merge together partcipants filter variables for both accuracy and time deletion
      values$out_participants <- c(values$sbj_accuracy$participant, 
                                   values$sbj_time$participant)
      # create teh condition for displaying the participants according to users' display configurations
      if(input$accuracy_del == 1 & input$sbjFast_del == 1){ # Display all participants
        # save the dataset with the results in a temporary dataframe values$display
        values$display <- values$dframe
        # compute the reliability only on the selected participants
        values$test_practice_rel <- cor(values$display[, c("d_practice", 
                                                           "d_test")])
        # display the descriptive statistics only for the selected participants
        values$desc_stats <- values$data[values$data$participant %in% 
                                           values$display$participant, ]
        
      } else if(input$accuracy_del == 2 & input$sbjFast_del == 1){ # Accuracy deletion only
        values$display <- values$dframe[!(values$dframe$participant) %in%
                                          values$sbj_accuracy$participant, ]
        
        values$out_400d <- values$dataset[!(values$dataset$participant) %in%
                                            values$sbj_accuracy$participant, ]
        
        values$out_400 <- sum(values$out_400d$fast400 == "no", na.rm = T)
        
        values$test_practice_rel <- cor(values$display[, c("d_practice", 
                                                           "d_test")])
        values$desc_stats <- values$data[values$data$participant %in% 
                                           values$display$participant, ]
      } else if (input$accuracy_del == 1 & input$sbjFast_del == 2){ # Fast participants deletion only
        values$display <- values$dframe[!(values$dframe$participant) %in%
                                          values$sbj_time$participant, ]
        
        values$out_400d <-  values$dataset[!(values$dataset$participant) %in%
                                             values$sbj_time$participant, ]
        
        values$out_400 <- sum(values$out_400d$fast400 == "no", na.rm = T)
        
        values$test_practice_rel <- cor(values$display[, c("d_practice", 
                                                           "d_test")])
        values$desc_stats <- values$data[values$data$participant %in% 
                                           values$display$participant, ]
      } 
      else if(input$accuracy_del == 2 & input$sbjFast_del == 2){ # Both accuracy and fast participants deletion
        values$display <- values$dframe[!(values$dframe$participant) %in%
                                          values$out_participants, ]
        
        values$out_400d <- values$dataset[!(values$dataset$participant) %in%
                                            values$out_participants, ]
        
        values$out_400 <- sum(values$out_400d$fast400 == "no", na.rm = T)
        
        values$test_practice_rel <- cor(values$display[, c("d_practice", 
                                                           "d_test")])
        
        values$desc_stats <- values$data[values$data$participant %in% 
                                           values$display$participant, ]
        
      }
      # create a label stating whether the fast trails cleaning was applied or not 
      if(values$d_select == 1 || values$d_select == 3 || values$d_select == 4){
        values$out_400 <- "Not expected for this D"
      } else {
        values$out_400 <- values$out_400
      }
      
    })
  
  # define the action for activating all the pop-up menu specified in the UI
  # All the pop-up menus are based on the click
  shinyjs::onclick("imp_text",
                   shinyjs::toggle(id = "details_imptext", anim = TRUE))
  
  shinyjs::onclick("imp_intro",
                   shinyjs::toggle(id = "details_intro", anim = TRUE))
  
  shinyjs::onclick("det_works",
                   shinyjs::toggle(id = "details_works", anim = TRUE))
  
  shinyjs::onclick("det_descriptive",
                   shinyjs::toggle(id = "details_descriptive", anim = TRUE))
  
  shinyjs::onclick("example_det",
                   shinyjs::toggle(id = "details_example", anim = TRUE))
  
  shinyjs::onclick("det_dpanel",
                   shinyjs::toggle(id = "details_dpanel", anim = TRUE))
  
  shinyjs::onclick("det_getting",
                   shinyjs::toggle(id = "details_getting", anim = TRUE))
  
  shinyjs::onclick("det_references",
                   shinyjs::toggle(id = "details_references", anim = TRUE))
  
  shinyjs::onclick("det_contacts",
                   shinyjs::toggle(id = "details_contacts", anim = TRUE))
  
  shinyjs::onclick("det_license",
                   shinyjs::toggle(id = "details_license", anim = TRUE))
  
  shinyjs::onclick("imp_det",
                   shinyjs::toggle(id = "details_import", anim = TRUE))
  
  shinyjs::onclick("sbjFast_det",
                   shinyjs::toggle(id = "details_sbjFast", anim = TRUE))
  
  shinyjs::onclick("take400_det",
                   shinyjs::toggle(id = "details_take400", anim = TRUE))
  
  shinyjs::onclick("builtin_det",
                   shinyjs::toggle(id = "details_builtin", anim = TRUE))
  
  shinyjs::onclick("graph_det",
                   shinyjs::toggle(id = "details_graph", anim = TRUE))
  
  shinyjs::onclick("select_D",
                   shinyjs::toggle(id = "details_D", anim = TRUE))
  
  shinyjs::onclick("info_prepare",
                   shinyjs::toggle(id = "details_prepare", anim = TRUE))
  
  shinyjs::onclick("practice_det_mapA",
                   shinyjs::toggle(id = "details_practice_mapA", anim = TRUE))
  
  shinyjs::onclick("test_det_mapA",
                   shinyjs::toggle(id = "details_test_mapA", anim = TRUE))
  
  shinyjs::onclick("practice_det_mapB",
                   shinyjs::toggle(id = "details_practice_mapB", anim = TRUE))
  
  shinyjs::onclick("test_det_mapB",
                   shinyjs::toggle(id = "details_test_mapB", anim = TRUE))
  
  shinyjs::onclick("mapA_det",
                   shinyjs::toggle(id = "details_mapA", anim = TRUE))
  
  shinyjs::onclick("mapB_det",
                   shinyjs::toggle(id = "details_mapB", anim = TRUE))
  
  shinyjs::onclick("built_det",
                   shinyjs::toggle(id = "details_built", anim = TRUE))
  
  shinyjs::onclick("point_det",
                   shinyjs::toggle(id = "details_point", anim = TRUE))
  
  shinyjs::onclick("hist_det",
                   shinyjs::toggle(id = "details_histogram", anim = TRUE))
  
  shinyjs::onclick("hist_det1",
                   shinyjs::toggle(id = "details_histogram1", anim = TRUE))
  
  shinyjs::onclick("accuracy_det",
                   shinyjs::toggle(id = "details_accuracy", anim = TRUE))
  
  shinyjs::onclick("percentage_det",
                   shinyjs::toggle(id = "details_perc", anim = TRUE))
  
  shinyjs::onclick("percentage_det1",
                   shinyjs::toggle(id = "details_perc1", anim = TRUE))
  # DSCORE output ####
  output$summary <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    # show the summary statistics
    dframe <- values$display
    summary(dframe[, c("d_practice", "d_test", "dscore")])
  })
  
  # Number of slow trials ####
  output$slow <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    dataset <- values$slow
    sum_slow <- nrow(dataset)
    # If there aren't slow trials, the "None" label is displayed
    ifelse(sum_slow != 0, sum_slow, "None" )
  })
  
  # Number of fast trials ####
  output$fast <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    
    out_400 <- values$out_400
    # If there aren't fast trials, the "None" label is displayed
    ifelse(out_400 == 0, "None", out_400)
  })
  
  # Number of participants deleted for the accuracy deletion ####
  output$mistakes <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    
    dataset <- values$sbj_accuracy
    num <- length(unique(dataset$participant))
    # If there aren't inaccurate participants, the "None" label is displayed
    ifelse(nrow(dataset) == 0, "None", num)
    
  })
  
  ## fast participants ##########
  output$sbjFast <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    
    sbj_fast <- values$sbj_time
    # If there aren't too fast participants, the "None" label is displayed
    ifelse(nrow(sbj_fast) == 0, "None", 
           length(unique(sbj_fast$participant)))
  })
  
  # Graphic displays ####
  output$distribution <- renderPlot({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    dframe <- values$display
    
    dframe <- dframe[, c("participant", "dscore")]
    
    library(ggplot2)
    # points #####
    if(input$graph == 1){
      if(input$point_opts == 1){
        # create the order for displaying participants
        values$type_graph <- "PointDefault"
        start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        dframe <- dframe[order(dframe$participant), ]
        dframe$dscore_cres <- dframe$participant
        dframe$dscore_cres <- as.factor(dframe$dscore_cres)
        # create a variable for teh position of the labels for the effect size according to the number of participants
        coordinates_labels <- ifelse(length(unique(dframe$participant)) < 150, 
                                     nrow(dframe)-1, 
                                     nrow(dframe)-10)
        # prepare the graph (points)
        g_graph <- ggplot(dframe,
                          aes(y = dscore, x = dscore_cres)) +
          geom_point(col = "springgreen4",  size = 2)
        g_graph <- g_graph + scale_x_discrete(name = "Participant",
                                              labels = dframe$participant)
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.40, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.41, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.70, label= "strong",
                                      col = "slateblue4" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.70, label= "strong",
                                      col = "slateblue4" )
        stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        
      } else if(input$point_opts == 2){
        # create the order for displaying participants
        values$type_graph <- "PointCrescent"
        start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        dframe <- dframe[order(dframe$dscore), ]
        dframe$dscore_cres <- 1:nrow(dframe)
        dframe$dscore_cres <- as.factor(dframe$dscore_cres)
        coordinates_labels <- ifelse(length(unique(dframe$participant)) < 150, 
                                     nrow(dframe)-1, nrow(dframe)-10 )
        # prepare the graph (points)
        g_graph <- ggplot(dframe,
                          aes(y = dscore, x = dscore_cres)) +
          geom_point(col = "springgreen4",  size = 2)
        g_graph <- g_graph + scale_x_discrete(name = "Participant",
                                              labels = dframe$participant)
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.40, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.41, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.70, label= "strong",
                                      col = "slateblue4" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels
                                      ,
                                      y = -0.70, label= "strong",
                                      col = "slateblue4" )
        stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        
      } else if(input$point_opts == 3){
        # create the order for displaying participants
        values$type_graph <- "PointDecrescent"
        start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        dframe <- dframe[order(dframe$dscore, decreasing = T), ]
        dframe$dscore_cres <- 1:nrow(dframe)
        dframe$dscore_cres <- as.factor(dframe$dscore_cres)
        coordinates_labels <- ifelse(length(unique(dframe$participant)) < 150, 
                                     (nrow(dframe)-(nrow(dframe)-2)), 
                                     (nrow(dframe)-(nrow(dframe)-15)) )
        # prepare the graph (points)
        g_graph <- ggplot(dframe,
                          aes(y = dscore, x = dscore_cres)) +
          geom_point(col = "springgreen4",  size = 2)
        g_graph <- g_graph + scale_x_discrete(name = "Participant",
                                              labels = dframe$participant)
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.21, label= "slight",
                                      col = "royalblue" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.40, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.41, label= "moderate",
                                      col = "orchid3" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = 0.70, label= "strong",
                                      col = "slateblue4" )
        g_graph <- g_graph + annotate("text", x= coordinates_labels,
                                      y = -0.70, label= "strong",
                                      col = "slateblue4" )
        stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
        
      }
      
      # graph specifications common to all the point graphs
      g_graph <- g_graph  + theme_classic()
      g_graph <- g_graph + theme(axis.text.x = element_text(size = 5))
      g_graph <- g_graph + geom_abline(slope = 0, intercept = 0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_abline(slope = 0, intercept = -0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_abline(slope = 0, intercept = 0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_abline(slope = 0, intercept = -0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_abline(slope = 0, intercept = 0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + geom_abline(slope = 0, intercept = -0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + ylab("D-score")
    }
    ## histogram ####
    else if(input$graph == 2){
      # Histogram graph
      values$type_graph <- "Histogram"
      start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
      g_graph <- ggplot(dframe,
                        aes(x = dscore)) +
        geom_histogram(bins = input$num.bin, col = "royalblue",  # number of bins depends on users' configuration
                       fill = "royalblue",
                       alpha = .50)
      g_graph <- g_graph  + theme_classic()
      g_graph <- g_graph + geom_vline( xintercept = 0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = 0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = 0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + annotate("text", x= 0.20,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= -0.10,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= 0.43,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.25,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.58,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + annotate("text", x= 0.71,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + xlab("D-score") + theme(axis.title.y = element_blank())
      stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
      
    }
    # density ####
    else if(input$graph == 3){
      # Density graph
      values$type_graph <- "Density"
      start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
      g_graph <- ggplot(dframe,
                        aes(x = dscore)) +
        geom_density(alpha = 0.70, fill = "seagreen" , col = "seagreen")
      g_graph <- g_graph  + theme_classic()
      g_graph <- g_graph + geom_vline( xintercept = 0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.15,
                                       col = "royalblue",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = 0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.35,
                                       col = "orchid3",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = 0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + geom_vline( xintercept = -0.65,
                                       col = "slateblue4",lty = 2, size = .50)
      g_graph <- g_graph + annotate("text", x= 0.20,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= -0.10,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= 0.43,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.25,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.58,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + annotate("text", x= 0.71,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + xlab("D-score") + theme(axis.title.y = element_blank())
      stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
    }
    # density + histogram #####
    else if(input$graph == 4){
      # density + histogram graph
      values$type_graph <- "HistDens"
      start_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
      g_graph <- ggplot(dframe,
                        aes(x = dscore)) +
        geom_histogram(aes(y=..density..), bins = input$num.bin1, # number of bins depends on users' configuration
                       col = "royalblue",
                       fill = "royalblue", alpha = .50)
      g_graph <- g_graph  + theme_classic()
      g_graph <- g_graph + geom_density(alpha = .70, col = "seagreen", 
                                        fill = "seagreen", trim =F)
      g_graph <- g_graph + geom_vline( xintercept = 0.15,
                                       col = "royalblue", lty = 2, size = .70)
      g_graph <- g_graph + geom_vline( xintercept = -0.15,
                                       col = "royalblue", lty = 2, size = .70)
      g_graph <- g_graph + geom_vline( xintercept = 0.35,
                                       col = "orchid3", lty = 2, size = .70)
      g_graph <- g_graph + geom_vline( xintercept = -0.35,
                                       col = "orchid3", lty = 2, size = .70)
      g_graph <- g_graph + geom_vline( xintercept = 0.65,
                                       col = "slateblue4", lty = 2, size = .70)
      g_graph <- g_graph + geom_vline( xintercept = -0.65,
                                       col = "slateblue4", lty = 2, size = .70)
      
      g_graph <- g_graph + annotate("text", x= 0.20,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= -0.10,
                                    y = -0.20, label= "slight", 
                                    col = "royalblue" )
      g_graph <- g_graph + annotate("text", x= 0.43,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.25,
                                    y = -0.20, label= "moderate",
                                    col = "orchid3" )
      g_graph <- g_graph + annotate("text", x= -0.58,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + annotate("text", x= 0.71,
                                    y = -0.20, label= "strong",
                                    col = "slateblue4" )
      g_graph <- g_graph + xlab("D-score") + theme(axis.title.y = element_blank())
      stop_time <- Sys.time() # it's needed for computing the waiting time for the shiny notification
    }
    values$g_graph <- g_graph # safe the graph in the reactive object
    sec_time <- as.numeric(stop_time - start_time) + 0.02 # compute the time needed for making each of the graph
    # prepare the shiny notification according to the time for doing the grpah
    withProgress(message = ifelse(input$update == 1,
                                  "Computing...", "Updating..."), 
                 value = input$update, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(sec_time)
                   }
                 })
    
    return(values$g_graph)
    
  })
  # toggle state for the download graph button (become active only when the values$g_graph object has something inside)
  observe({
    shinyjs::toggleState("down_plot", !(is.null(values$g_graph)))
  })
  
  # single graph indicator ####
  output$click_info <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$plot1_click , "Click on a point")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$plot1_click, "Click on a point")
      ) 
    }
    
    dframe <- values$display
    
    dframe <- dframe[, c("participant", "dscore")]
    # specificy the order of participants for getting the correct point iun the dataframe
    if(input$point_opts == 1){
      dframe <- dframe[order(dframe$participant), ]
      dframe$dscore_cres <- dframe$participant
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    } else if(input$point_opts == 2){
      dframe <- dframe[order(dframe$dscore), ]
      dframe$dscore_cres <- 1:nrow(dframe)
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    } else if(input$point_opts == 3){
      dframe <- dframe[order(dframe$dscore, decreasing = T), ]
      dframe$dscore_cres <- 1:nrow(dframe)
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    }
    # select the participant corresponding to the point
    single_id <- nearPoints(dframe, input$plot1_click, addDist = F)
    rownames(single_id) <- NULL
    single_id <- single_id[,1:2]
    validate(
      need(nrow(single_id) != 0, "Click on a point")
    )
    single_id
  })
  
  
  # multiple indicator in graph #####
  output$brush_info <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$plot1_brush , "Highlight graph area")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$plot1_brush, "Highlight graph area")
      ) 
    }
    
    dframe <- values$display
    
    dframe <- dframe[, c("participant", "dscore")]
    # specificy the order of participants for getting the correct point iun the dataframe
    if(input$point_opts == 1){
      dframe <- dframe[order(dframe$participant), ]
      dframe$dscore_cres <- dframe$participant
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    } else if(input$point_opts == 2){
      dframe <- dframe[order(dframe$dscore), ]
      dframe$dscore_cres <- 1:nrow(dframe)
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    } else if(input$point_opts == 3){
      dframe <- dframe[order(dframe$dscore, decreasing = T), ]
      dframe$dscore_cres <- 1:nrow(dframe)
      dframe$dscore_cres <- as.factor(dframe$dscore_cres)
      
    }
    # select the participants in the selected area
    brush_id <- brushedPoints(dframe, input$plot1_brush)
    validate(
      need(nrow(brush_id) != 0, "Highlight graph area")
    )
    rownames(brush_id) <- NULL
    brush_id[,1:2]
  })
  
  # ## histogram indicator #####
  output$info_hist <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$plot1_brush , "Highlight graph area")
      )
    } else if (input$reset > 0){
      validate(
        need(input$plot1_brush, "Highlight graph area")
      )
    }
    
    dframe <- values$display
    
    dframe <- dframe[, c("participant", "dscore")]
    # select the minimum and the maximu of the selected area and theri corresponding points in teh dataframe
    dframe$new <- (dframe$dscore > input$plot1_brush$xmin &
                     dframe$dscore < input$plot1_brush$xmax)
    d_plot <- dframe[which(dframe$new == T), ]
    d_plot <- d_plot[order(d_plot$dscore), ]
    rownames(d_plot) <- NULL
    d_plot <- d_plot[, 1:2]
    d_plot
    
  })
  ## test pratice reliability #####
  output$pt_reliability <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    test_practice_rel <- values$test_practice_rel
    round(test_practice_rel[2,1],2)
  })
  
  
  # ## mean block ####
  output$mean.block <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    # Compute the average response time in each condition
    dataset <- values$desc_stats
    latency_descript <- with(dataset, 
                             aggregate(latency_cor, 
                                       by = list(Condition), 
                                       FUN = summary))
    # Compute the average response time in the pratice and test blocks
    latency_descript <- rbind(latency_descript, 
                              with(dataset, 
                                   aggregate(latency_cor, 
                                             by = list(block_pool), 
                                             FUN = summary)))
    # Compute the average response time in each block
    latency_descript <- rbind(latency_descript,
                              with(dataset, 
                                   aggregate(latency_cor, 
                                             by = list(blockR), 
                                             FUN = summary)))
    
    colnames(latency_descript) <- gsub('x', '', 
                                       colnames(latency_descript))
    colnames(latency_descript)[1] <- ""
    for(i in 2:ncol(latency_descript)){
      latency_descript[,i] <- round(latency_descript[,i], 2)
    }
    latency_descript
  })
  
  
  #  # ## accuracy block ####
  output$accuracy_block <- renderPrint({
    # Display a message while waiting for data
    if(input$reset == 0){
      validate(
        need(input$update > 0 , "Waiting for data")
      ) 
    } else if (input$reset > 0){
      validate(
        need(input$update > 0  && 
               (!is.null(values$display)), "Waiting for data")
      ) 
    }
    dataset <- values$desc_stats
    # Compute the proportion of correct responses in each condition
    accuracy_descript <- with(dataset, 
                              aggregate(correct, 
                                        by = list(Condition), 
                                        FUN = mean))
    # Compute the proportion of correct responses in pratice and test blocks
    accuracy_descript <- rbind(accuracy_descript, 
                               with(dataset, 
                                    aggregate(correct, 
                                              by = list(block_pool), 
                                              FUN = mean)))
    # Compute the proportion of correct responses in each block
    accuracy_descript <- rbind(accuracy_descript, 
                               with(dataset, 
                                    aggregate(correct, 
                                              by = list(blockR), 
                                              FUN = mean)))
    
    colnames(accuracy_descript) <- c("", "Proportion_correct")
    accuracy_descript$Proportion_correct <- round(
      accuracy_descript$Proportion_correct,2)
    accuracy_descript
  })
  
  #  DOWNLOAD Results ####
  output$downloadData <- downloadHandler(
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("ShinyAPPDscore",input$sel_d, ".csv", sep = "")
    },
    content = function(file) {
      sbj_data_wide <- values$descript_data
      write.table(sbj_data_wide, file, sep = ",",
                  row.names = FALSE)
    }
  )
  #
  
  # reset app ####
  observeEvent(input$reset | !is.null(values$alert), 
               {
                 shinyjs::reset("Dapp")
                 values$ready <-NULL
                 values$dataset <-NULL
                 values$descript <-NULL
                 values$data <- NULL
                 values$display <- NULL
                 values$out_400 <- NULL
                 values$sbj_accuracy <- NULL
                 values$sbj_time <- NULL
                 values$test_practice_rel <- NULL
                 values$g_graph <- NULL
                 
               })  
  
  # Download graphs ####
  output$down_plot <- downloadHandler(
    filename = function(){
      paste(values$type_graph, "Dscore",input$sel_d, ".pdf", sep = "")
    }, 
    content = function(file){
      graph <- values$g_graph + theme(plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"))
      ggsave(file, plot = graph, width = 15, height = 7)
      
    }
  )
  
  
  # DOWNLOAD template ####
  
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("templateDshiny", ".csv", sep = "")
    },
    content = function(file) {
      
      data <- data.frame(matrix(nrow=1, ncol = 4))
      data[is.na(data)] <- ""
      colnames(data) <- c("participant", "block", "correct",
                          "latency")
      write.table(data, file, sep = ",", row.names = FALSE)
    }
  )
}