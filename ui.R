library(shiny) # load shiny package
library(shinythemes) # load shinythemes to change the app theme
library(shinyjs) # load shinyjs package to use jav script elements


# Define UI for dataset viewer app ----
ui <- fluidPage(
  theme = shinytheme("cosmo"), # define theme
  shinyjs::useShinyjs(), # call shinyjs package
  # Define CSS for the app
  tags$head(tags$style(
    HTML(
      "
                                  @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');

                                  h1 {
                                  font-weight: 500;
                                  line-height: 1.1;
                                  color: #93baff;
                                  }
                                  h3 {
                                  font-style: italic;
                                  color: #5e67e5;
                                  text-align: center;
                                  }
                                  h4 {
                                  font-style: italic;
                                  color: 	#5e67e5;
                                  font-weight: 400;
                                  }
                                  h5 {
                                  font-style: italic;
                                  color: #7a6db1;
                                  font-weight: 450;
                                  }
                                  em {
                                  font-weight:italic;
                                  }
                                  table, th, td {
                                  border: 1px solid black;
                                  border-collapse: collapse;
                                  padding: 8px;
                                  }
                                  blockquote{
                                  font-size: 12pt;
                                  }

                                  .btn {
                                  background-color:#b491f6;

                                  }
                                  .shiny-notification{
                                    position: fixed;
                                    top: 33%;
                                    left: 53%;
                                    right: 33%;
                                    background-color:#c7ccff;
                                    color: #7a6db1;
}
                                  "
    )
  )),
  div(
    id = "Dapp", # app ID
    headerPanel("DscoreApp"), # app Title
    
    sidebarLayout(
      # Sidebar panel for inputs 
      sidebarPanel(
        # Pop-up menu for the toy dataset checkbox  
        a(id = "example_det", "Example data", href = "#"),
        shinyjs::hidden(div(
          id = "details_example",
          helpText(
            "Check this box to use an example Race IAT dataset for computing",
            "the D-score."
          )
        )),
        # Checkbox for the toy dataset (default = F)
        checkboxInput("checkbox", "Race IAT dataset",
                      value = FALSE),
        # Conditional Panel for importing users' dataframe 
        conditionalPanel(
          condition = "input.checkbox == false",
          # data import pop-up menu
          a(id = "imp_det", "Choose CSV file", href = "#"),
          shinyjs::hidden(div(
            id = "details_import",
            helpText(
              "Import data. CSV separator must be comma (,).",
              "Please use the template from READ ME FIRST",
              "compiled according to instructions."
            )
          )),
          # Users' data impot button
          fileInput(
            'datafile',
            '',
            accept = c('text/csv', 'text/comma-separated-values,text/plain')
          )
        ),
        
        # Block labels definition pop-up menu 
        fluidRow(
          column(
            5,
            a(id = "practice_det_mapA", "MappingA Practice block label", href = "#"),
            shinyjs::hidden(div(
              id = "details_practice_mapA",
              helpText("How did you label the MappingA pratice block?")
            )),
            # actual labels for the block, taken from the dataset
            uiOutput("label_mapA_practice")
            
          ),
          column(
            5,
            a(id = "test_det_mapA", "MappingA Test block label", href = "#"),
            shinyjs::hidden(div(
              id = "details_test_mapA",
              helpText("How did you label the MappingA test block?")
            )),
            uiOutput("label_mapA_test")
          )
        ),
        
        fluidRow(
          column(
            5,
            a(id = "practice_det_mapB", "MappingB Practice block label", href = "#"),
            shinyjs::hidden(div(
              id = "details_practice_mapB",
              helpText("How did you label the MappingB pratice block?")
            )),
            uiOutput("label_mapB_practice")
            
          ),
          column(
            5,
            a(id = "test_det_mapB", "MappingB Test block label", href = "#"),
            shinyjs::hidden(div(
              id = "details_test_mapB",
              helpText("How did you label the MappingB pratice block?")
            )),
            uiOutput("label_mapB_test")
          )
          
        ),
        # Upload data button
        fluidRow(
          column(4,
                 actionButton("load", "Prepare Data")),
          # Pop-up menu for the uploading button and the Data ready message
          column(
            4,
            a(id = "info_prepare", "Show info", href = "#"),
            shinyjs::hidden(div(
              id = "details_prepare",
              helpText(
                "Upload and prepare data for calculating the D score \n
                  When it's finished a message will appear next the button"
              )
            ))
            
          ),
          # Data are ready message
          column(4,
                 uiOutput("data_ready"))
          
        ),
        
        # D-score selection pop-up menu 
        a(id = "select_D", "Show info", href = "#"),
        shinyjs::hidden(div(
          id = "details_D",
          helpText("Which D would you like to compute?")
        )),
        # D-score selection
        selectInput(
          "sel_d",
          label = "",
          list(
            "Select your D" = 0,
            "BUILT-IN" = c(
              "D1 (Built-in, no lower treatment)" = 1,
              "D2 (Built-in, 400ms lower treatment)" = 2
            ),
            "NO BUILT-IN" = c(
              "D3 (+2sd error inflation, no lower treament)" = 3,
              "D4 (+600ms error inflation, no lower treatment)" = 4,
              "D5 (+2sd error inflation, 400ms lower treatment)" = 5,
              "D6 (+600ms error inflation, 400ms lower treatment)" = 6
            )
          )
        ),
        # Accuracy deletion pop-up menu
        fluidRow(
          column(
            4,
            a(id = "accuracy_det", "Accuracy Cleaning", href = "#"),
            shinyjs::hidden(div(
              id = "details_accuracy",
              helpText(
                "Apply cleaning strategy based on accuracy deletion?",
                "Practice + Test blocks: the % of incorrect response,",
                "is computed on the trials of both blocks",
                "Only Test blocks: the % of incorrect responses",
                "is computed only on Test block trials"
              )
            )),
            # Accuracy deletion selection (default = 1 = No deletion selected)
            radioButtons(
              "accuracy_del",
              label = "",
              choices = list("No" = 1, "Yes (Practice + Test blocks)" = 2),
              
              selected = 1
            )
          ),
          # Error percentage threshold pop-up menu
          column(
            3,
            conditionalPanel(
              condition = "input.accuracy_del == '2'",
              a(id = "percentage_det", "Error Percentage", href = "#"),
              shinyjs::hidden(div(
                id = "details_perc",
                helpText("Choose the error percentage you are willing to accept")
              )),
              # error percentage selection
              numericInput(
                "perc_error",
                label = "",
                value = 25,
                min = 5,
                max = 40
              )
            )
          ),
          # Fast participants deletion pop-up menu
          column(
            3,
            a(id = "sbjFast_det", "Fast participants cleaning", href = "#"),
            shinyjs::hidden(div(
              id = "details_sbjFast",
              helpText(
                "Eliminate participants for whom more than 10% of
                                                trials have latency less
                                                than 300 ms"
              )
            )),
            # Fast participants deletion selection (default = 1 = No deletion selected)
            radioButtons(
              "sbjFast_del",
              label = "",
              choices = list("No" = 1, "Yes" = 2),
              selected = 1
            )
          )
        ),
        # Graphic display of the results (appears only when a D-score is selected)
        conditionalPanel(
          condition = "input.sel_d != '0'",
          # Graphic selection pop-up menu
          fluidRow(
            column(
              4,
              a(id = "graph_det", "Graphic display", href = "#"),
              shinyjs::hidden(div(
                id = "details_graph",
                helpText("How would you display your D-scores?")
              )),
              # Graphic selection (default = Points graph)
              radioButtons(
                "graph",
                label = "",
                choices = list(
                  "Points" = 1,
                  "Histogram" = 2,
                  "Density" = 3,
                  "Histogram + Density" = 4
                ),
                selected = 1
              )
            ),
            # Conditional panel for the participants' display order in the Point graph
            column(
              5,
              conditionalPanel(
                condition = "input.graph == '1'",
                # Participants' display order in the Point graph pop-up menu
                a(id = "point_det", "Point Graph", href = "#"),
                shinyjs::hidden(div(
                  id = "details_point",
                  helpText("How would you like to order the Participants?")
                )),
                # Participants' display order in the Point graph selection (default = None)
                selectInput(
                  "point_opts",
                  "",
                  choices = list(
                    "None" = 1,
                    "D-score:Increasing" = 2,
                    "D-score:Decreasing" = 3
                  ),
                  selected = 1
                )
              )
            )
          ),
          # Conditional panel for number of bins in the histogram graph
          conditionalPanel(
            condition = "input.graph == '2'",
            # Number of bins the histogram graph pop-up menu
            a(id = "hist_det", "Histogram number of bins", href = "#"),
            shinyjs::hidden(div(
              id = "details_histogram",
              helpText("Select the number of bins you want")
            )),
            # Number of bins the histogram graph selection (default = 30)
            sliderInput(
              "num.bin",
              label = "",
              min = 1,
              max = 100,
              step = 1,
              value = 30
            )
          ),
          # Conditional panel for number of bins in the histogram + density graph
          conditionalPanel(
            condition = "input.graph == '4'",
            # Number of bins the histogram + density graph pop-up menu
            a(id = "hist_det1", "Histogram number of bins", href = "#"),
            shinyjs::hidden(div(
              id = "details_histogram1",
              helpText("Select the number of bins you want")
            )),
            # Number of bins the histogram + density graph selection (default = 30)
            sliderInput(
              "num.bin1",
              label = "",
              min = 1,
              max = 100,
              step = 1,
              value = 30
            )
          )
        ),
        
        # Include clarifying text
        helpText("Note: Please, read the READ ME FIRST before doing anything"),
        # D-score calcution/update button
        actionButton("update", "Calculate & Update"),
        # Restart app button
        actionButton("reset", "Reset & Restart"),
        # Download results button
        downloadButton("downloadData", "Download")
      ),
      
      # Main panel 
      mainPanel(tabsetPanel(
        tabPanel(
          h4("READ ME FIRST"),
          # Introduction pop-up menu
          a(id = "imp_intro", h3("The D-score Shiny App"), href = "#"),
          shinyjs::hidden(div(
            id = "details_intro",
            HTML(
              "
                                 <blockquote>
                                 <p>
                                 This app will help you in computing different
                                 <em>D-score</em>s for the Implicit Association
                                 Test (IAT; Greenwald et al., 1998), according to Greenwald, Nosek and
                                 Banaji (2003). Beyond the computation of the
                                 <em>D-score</em>s <em>per se</em>, the
                                 DShinyApp app generates different graphic
                                 representations which will allow you to observe
                                 how the <em>D-score</em>s change according to
                                 the selected <em>D</em> (e.g.,
                                 the different strategies for the error
                                 replacement), and the different settings you
                                 specified.
                                 <p>
                                 Within the features offered by the app, you can
                                 decide whether to discard participants with
                                 a high proportion of incorrect responses
                                 (Nosek, Banaji &#38; Greenwald, 2002), or
                                 participants with a high percentage of fast
                                 responses (Greenwald et al., 2003), or both.
                                 At the end of your computation, you can download
                                 a file containing all of your participants'
                                 <em>D-score</em>s for further analyses.
                                 <br>
                                 In the following sections, details on the
                                 functioning of the app and its features are
                                 provided.
                                 </p>

                                 </blockquote>"
            )
          )),
          # Import pop-up menu
          a(id = "imp_text", h3("Import Data"), href = "#"),
          shinyjs::hidden(div(
            id = "details_imptext",
            HTML(
              "

                                 <blockquote>
                                 <p>
                                 Before importing the data&#58;
                                 </p>
                                 <ul>
                                 <li> Remove from the dataset the pure practice
                                 blocks of the IAT (i.e., the blocks in which
                                 only either the target or the attirbute stimuli
                                 are sorted in their reference categories).</li>
                                 <li> The IAT data are in a CSV file with
                                 &#34;,&#34; set as separator of the columns.
                                 In the template downloadable at
                                 &#34;Download CSV Template&#34;, &#34;,&#34;
                                 is already set as the column separator.</li>
                                 <li> Rename the columns according to the
                                 columns' names of the Template file, and define
                                 the variables as follows&#58;</li>

                                 <ul>
                                 <li> <b> participant&#58;</b> it defines the
                                 ID of the participants. The IDs may be either
                                 numeric (e.g., 1,2,..300...450) or a string
                                 (e.g., ss01, aa05, JohnDoe1001 etc.). </li>
                                 <li> <b> block&#58; </b> It defines the blocks
                                 of the IAT. The labels identifying each block
                                 are not importart <em>per se</em>. The important
                                 thing is that each block is defined by a unique
                                 label, hence there have to be <em><strong>four
                                 distinct labels</strong></em> defining the
                                 <em>practice</em> and <em>test </em> blocks of
                                 <strong> Mapping A </strong>(e.g.,
                                 practiceWhiteGood, testWhiteGood) and the
                                 <em>practice</em> and <em>test</em> block of
                                 <strong> Mapping B</strong> (e.g.,
                                 practiceWhiteBad, testWhiteBad). </li>
                                 <li> <b> latency&#58; </b> It contains the
                                 latencies of the responses expressed in
                                 millisecond. If the IAT <b>DID NOT include a
                                 built-in</b> correction, place the raw
                                 latencies in this variable. If the IAT
                                 <b>DID include a built-in</b> correction,
                                 please place the <b>already inflated</b>
                                 latency of the error responses.</li>
                                 <li><b> correct&#58; </b> It contains the correct
                                 and error responses to the IAT. Correct responses
                                 have to be coded as 1, error responses have to
                                 be coded as 0. </li>
                                 </ul>
                                 </ul> <br>
                                 Summarizing, for using the App it is fundamental
                                 that the dataset contains the four abovementioned
                                 variables with the specific associated names.
                                 </blockquote>"
            ),
            # Download template button
            fluidRow(column(
              3,
              offset = 8,
              downloadButton("downloadTemplate", "Download CSV Template")
            ))
          )),
          # App functioning pop-up menu
          a(id = "det_works", h3("How it works"), href = "#"),
          shinyjs::hidden(div(
            id = "details_works",
            HTML(
              "
                                <blockquote>
                                <p> The app is provided with a toy dataset
                                containing data from a Race IAT. If you check
                                the &#34;Race IAT dataset&#34; box, the data
                                will be automatically loaded in the server, and
                                the <em>D-score</em> can be computed.
                                Otherwise, you can import your dataset by
                                following these instructions:</p>
                                <ul>
                                <li> Use the <b>&#34;Browse&#34;</b> function
                                to select your data. </li>
                                <li> Select the labels of the <em>practice</em>
                                and <em>test</em> blocks of both <b>Mapping A</b>
                                and <b>Mapping B</b> from the dropdown menu.
                                The dropdown menu for each of
                                the four levels defining the blocks will display
                                the labels in your dataset for each of the
                                <em>practice</em> and <em>test</em> blocks of
                                both <strong>Mapping A</strong> and
                                <strong>Mapping B</strong>.</li>
                                <li> Once the labels identifying
                                the correct blocks are selected, the <b>&#34;Prepare data&#34;</b>
                                button is activated. Click on the
                                <b>&#34;Prepare data&#34;</b> button and wait
                                for the alert <b>&#34;Data are ready&#33;&#34;</b>
                                to appear right next to the button itself. <br>
                                If there is something wrong with the block labels,
                                an alert message will appear. In such cases,
                                please check carefully the labels in your dataframe and restart the
                                app.</li>
                                <li> At this point, data are ready for the
                                computation of the <em>D-score</em>. The
                                following table contains the computation details for
                                each <em>D-score</em>.
                                If your data <b>DO contain a built-in</b>
                                correction, you can choose <b> only
                                </b>between <em>D1</em> and <em>D2</em>. If
                                your data <b> DO NOT contain a built-in
                                </b>correction, you can choose between
                                all the other <em>D-score</em>s.</li>
                                </ul>
                                </blockquote>"
            ),
            # D-scores table
            HTML(
              "<blockquote>
                                 <center>
                                 <table>
                                 <tr>
                                 <th>Dscore</th>
                                 <th>Error inflation</th>
                                 <th>Lower tail treatment</th>
                                 </tr>
                                 <tr>
                                 <td>D1</td>
                                 <td>Built-in correction</td>
                                 <td>No</td>
                                 </tr>
                                 <tr>
                                 <td>D2</td>
                                 <td>Built-in correction</td>
                                 <td>delete trials &lt; 400 ms</td>
                                 </tr>
                                 <tr>
                                 <td>D3</td>
                                 <td>Replace errors: mean (correct responses) + 2sd</td>
                                 <td>No</td>
                                 </tr>
                                 <tr>
                                 <td>D4</td>
                                 <td>Replace errors: mean (correct responses) + 600 ms</td>
                                 <td>No</td>
                                 </tr>
                                 <tr>
                                 <td>D5</td>
                                 <td>Replace errors: mean (correct responses) + 2sd</td>
                                 <td>delete trials &lt; 400 ms</td>
                                 </tr>
                                 <tr>
                                 <td>D6</td>
                                 <td>Replace errors: mean (correct responses) + 600 ms</td>
                                 <td>delete trials &lt; 400 ms</td>
                                 </tr>
                                 </table>
                                 </center>
                                 <br>
                                 <b>&#34;Dscore&#34;</b> refers to the
                                 <em>D-score</em> you can find in the
                                 <b>&#34;Select your D&#34;</b> dropdown menu,
                                 <b>&#34;Error inflation&#34;</b> refers to the
                                 treatment for the error responses, and
                                 <b>&#34;Lower tail treatment&#34;</b> is the
                                 tretament for the fast responses. For all the
                                 <em>D-score</em> procedures, the responses with
                                 a latency over 10,000 ms have been discarded.
                                 </br>

                                 For any further details on the different
                                 strategies for the <em>D-score</em>
                                 computation, please refer to Greenwald et al.
                                 (2003).
                                 </p>

                                 <p>
                                 The D-score is computed as <b>Mapping B</b> -
                                 <b>Mapping A</b>.
                                 </blockquote>"
            ),
            
            HTML(
              " <blockquote>
                                 <ul>
                                 <li> Once a <em>D-score</em> is selected from the
                                 dropdown menu, the
                                 <b>&#34;Compute &#38; Update&#34;</b> button
                                 becomes usable.
                                 </br>When the <b>&#34;Compute &#38; Update&#34;</b>
                                 button is clicked, the results appear in the
                                 &#34;<em>D-score</em> results&#34; panel. </li>
                                 <li> You can decide to display all participants'
                                 <em>D-scores</em> (default) or to exclude the
                                 ones with an high percentage of error responses
                                 (<b>&#34;Accuracy deletion&#34;</b> option) or
                                 a high rate of fast responses
                                 (<b>&#34;Fast participants cleaning&#34;</b>
                                 option), or both. </li>
                                 Whether you decide to display all the
                                 participants' <em>D-score</em> or not, ALL the
                                 participants will be included in the
                                 downloadable file.
                                 <li> Everytime you make a change, remember to
                                 click on the
                                 <b>&#34;Compute &#38; Update&#34;</b>,
                                 otherwise no changes will happen.</li>
                                 <li>When you want to download your file, click
                                 on the <b>&#34;Download&#34;</b> button.
                                 This file will contain the <b>lastly computed
                                 <em>D-score</em></b>. For further information
                                 on the downloadable file, please read the
                                 <em>&#34;What you get section&#34;</em>.</li>
                                 </ul>
                                 </blockquote>"
            )
          )),
          # Results panel pop-up menu
          a(id = "det_dpanel", h3("The D-score results panel"), href = "#"),
          shinyjs::hidden(div(
            id = "details_dpanel",
            HTML(
              "
                                 <blockquote>

                                 <ul>
                                 <li> <b><em>D-score</em>&#58;</b> Graphics
                                 display of the <em>D-score</em>s of the
                                 participants, according to the specified
                                 options. </li>
                                 <li> <b> Point (Only for point graph)&#58;</b>
                                 Click on a point of the graph: The participant's
                                 <em>D-score</em> and ID corresponding to the point
                                 will appear in the box. </li>
                                 <li> <b> Area (for all the other graphic
                                 display)&#58;</b> Highlight an area of the graph:
                                 The <em>D-score</em>s and IDs of the Participants
                                 included in the selected area will appear in the
                                 box. </li>
                                 <li> <b> Summary&#58;</b> Displays the summary
                                 statistics (<em>Minimum, 1st quartile, Median,
                                 Mean, 3rd quartile, Maximum</em>) of the
                                 practice and test blocks <em>D</em>s, along
                                 with the actual <em>D-score</em>. </li>
                                 <li> <b> Trials &#62; 10,000 ms&#58; </b>
                                 Displays the number of trials with a
                                 latency &#62; 10,000 ms.</li>
                                 <li> <b> Trials &#60; 400 ms&#58; </b>Displays
                                 the number of trials with a latency &#60; 400 ms.
                                 If the selected <em>D-score</em> did not include
                                 the lower tail treamentment, the message
                                 &#34;Not expected for this D&#34; will appear
                                 in the box.</li>
                                 <li> <b> Accuracy deletion&#58;</b> Appears
                                 only if the <b>&#34;Accuracy deletion&#34;</b> option is selected.
                                 It displays the number of Participants (if any)
                                 discarded because of an error response percentage
                                 greater than a given percentage
                                 (default 25&#37;). </li>
                                 <li> <b> Participants &#60; 300 ms&#58; </b>
                                 Appears only when <b>&#34;Fast participants cleaning&#34;</b> is
                                 selected. It displays the number of participants
                                 with more than 10&#37; of responses under 300
                                 ms.</li>
                                 <li> <b> Practice-Test reliability&#58; </b>
                                 Displays the IAT reliability computed as the
                                 correlation between the <em>D</em>s in the
                                 practice and test blocks (Gawronski et al., 2017). </li>
                                 </ul>
                                 </blockquote>"
            )
          )),
          # Descriptive statistics pop-up menu
          a(
            id = "det_descriptive",
            h3("Descriptive statistics panel"),
            href = "#"
          ),
          shinyjs::hidden(div(
            id = "details_descriptive",
            HTML(
              "
                                 <blockquote>
                                 The third and last panel of the App includes
                                 the descriptive statistics of the data, always
                                 updated to the last settings specified:
				 <p>
                                 <ul>
                                 <li><b> Average response time:</b> It
                                 contains the descriptive statistics (<em>
                                 Minimum, 1st quartile, Median, Mean, 3rd
                                 quartile, Maximum </em>) of the response times
                                 in the two mapping conditions
                                 (<b>&#34;mappingA&#34;</b>, and
                                 <b>&#34;mappingB&#34;</b>, first two rows), in
                                 practice and test blocks
                                 (<b>&#34;practice&#34;</b> and
                                 <b>&#34;test&#34;</b>, third and fourth row),
                                 and in the four blocks of the IAT
                                 (<b>&#34;practice.MappingA&#34;</b>,
                                 <b>&#34;practice.MappingB&#34;</b>,
                                 <b>&#34;practice.MappingB&#34;</b>,
                                 <b>&#34;test.MappingB&#34;</b>).</li>
                                 <li><b> Accuracy: </b> Contains the
                                 proportion of correct responses
                                 (&#34;Proportion_correct&#34;) in the two
                                 mapping conditions (<b>&#34;mappingA&#34;</b>,
                                 and <b>&#34;mappingA&#34;</b>, first two rows),
                                 in practice and test blocks
                                 (<b>&#34;practice&#34;</b> and
                                 <b>&#34;test&#34;</b>, third and fourth row),
                                 and in the four blocks of the IAT
                                (<b>&#34;practice.MappingA&#34;</b>,
                                <b>&#34;practice.MappingB&#34;</b>,
                                <b>&#34;practice.MappingB&#34;</b>,
                                <b>&#34;test.MappingB&#34;</b>).</li>
                                 </ul>
				</p>
                                 </blockquote>"
            )
          )),
          # Downloadable file pop-up menu
          a(id = "det_getting", h3(" What you get"), href = "#"),
          shinyjs::hidden(div(
            id = "details_getting",
            HTML(
              "

                                 <blockquote>
                                 <p>
                                 The CSV you will obtain contains the following
                                 information. Each column refers
                                 to the observed values for each participant.
                                 </p>

                                 <ul>
                                 <li><b>participant&#58;</b> Participants ID. </li>
                                 <li><b>n_trial&#58;</b> IAT total number of
                                 trials<b> before data cleaning</b>. </li>
                                 <li><b>slow10000&#58;</b> &#35; of trials
                                 &#62; 10,000 ms. </li>
                                 <li><b>num.300&#58;</b> &#35; of trials
                                 &#60; 300 ms. </li>
                                 <li> <b>num.400&#58;</b> &#35; of trials
                                 &#60; 400 ms. </li>
                                 <li><b>mean.tot&#58;</b> Overall mean
                                 latency for each participant. </li>
                                 <li><b>p_correct_block.practice.MappingA&#58;</b>
                                 Proportion of correct responses in practice
                                 block of Mapping A. </li>
                                 <li><b>p_correct_block.practice.MappingB&#58;</b>
                                 Proportion of correct responses in practice block
                                 of Mapping B. </li>
                                 <li><b>p_correct_block.test.MappingA&#58; </b>
                                 Proportion of correct responses in test block
                                 of Mapping A. </li>
                                 <li><b>p_correct_block.test.MappingB&#58; </b>
                                 Proportion of correct responses in test block
                                 of Mapping B. </li>
                                 <li><b>p_correct_bpool.practice&#58; </b>
                                 Proportion of correct responses practice blocks
                                 (Mapping A + Mapping B)</li>
                                 <li><b>p_correct_bpool.test&#58; </b>
                                 Proportion of correct responses test blocks
                                 (Mapping A + Mapping B).</li>
                                 <li><b>prop_correct_cond_MappingA&#58; </b>
                                 Proportion of correct responses in Mapping A.</li>
                                 <li><b>prop_correct_cond_MappingB&#58; </b>
                                 Proportion of correct responses in Mapping B. </li>
                                 <li><b>p_correct_tot&#58; </b>Overall
                                 proportion of correct responses. </li>
                                 <li><b>d_practice.&#35;&#58;
                                 </b><em>D-score</em> computed on practice blocks.</li>
                                 <li><b>d_test.&#35;&#58; </b><em>D-score</em>
                                 computed on test blocks. </li>
                                 <li><b>dscore.&#35;&#58; </b><em>D-score</em>.</li>
                                 <li><b>cond_ord&#58; </b>Order of condition
                                 presentation. </li>
                                 <li><b>LegendMappingA&#58; </b> Informs you
                                 about what &#34;MappingA &#34;corresponds
                                 according to your labels.</li>
                                 <li><b>LegendMappingB&#58;</b> Informs you
                                 about what &#34;MappingB &#34;corresponds
                                 according to your labels.</li>
                                 </ul>
                                 The &#35; next to &#34;d_test&#34;,
                                 &#34;d_practice&#34;, and &#34;dscore&#34;
                                 indicates the number corresponding to the last
                                 computed <em>D-score</em>. For instance, if you
                                 lastly selected the &#34;D5&#34;, a &#34;5&#34;
                                 will be printed next to the labels,
                                 resulting in &#34;d_test<b>.5</b>&#34;,
                                 &#34;d_practice<b>.5</b>&#34;,
                                 and &#34;dscore<b>.5</b>&#34;.
                                 </blockquote>"
            )
          )),
          # Final remarks, references, Contact, and License pop-up menu
          a(
            id = "det_final",
            h3("Final remarks, References, Contacts, and License"),
            href = "#"
          ),
          shinyjs::hidden(div(
            id = "details_final",
            HTML(
              "
              <blockquote>
              <p>
              Please refer to the <a href=https://implicit.harvard.edu/implicit/demo/copyright.html>Project Implicit Website</a> for the 
              interpretation guidelines of the <em>D-score</em>s effect size
              reported in the graphs.
              </p>
              </blockquote>
              

                                 <blockquote>
                                 <p>
                                 The App can be resetted by clicking on the
                                 <b> &#34;Reset &#38; Restart&#34;</b> button.
                                 </p>
                                 <h4><b>References</b></h4>
                                 <p>
                                 Gawronski, B., Morrison, M., Phills, C. E., & Galdi, S. (2017). Temporal stability of 
                                 implicit and explicit measures:
                                 A longitudinal analysis. <em> Personality and Social Psychology Bulletin, 43</em>(3), 300-312.
                                 doi: 10.1177/0146167216684131
                                 </p>
                                 <p>
                                 Greenwald, A. G., McGhee, D. E., & Schwartz, J. L. K. (1998). Measuring Individual Differences in 
                                 Implicit Cognition: The Implicit Association Test. <em>Journal of Personality and Soclal Psychology, 
                                 74</em>(6), 
                                 1464–1480. doi: 10.1037/0022-3514.74.6.1464
                                 </p>
                                 <p>
                                 Greenwald, A. G., Nosek, B. A., &#38; Banaji,
                                 M. R. (2003). Understanding and using the
                                 implicit association test&#58; I. An improved
                                 scoring algorithm. <em>Journal of personality
                                 and social psychology, 85</em>(2), 197-216.
                                 doi: 10.1037/0022-3514.85.2.197
                                 </p>
                                 <p>
                                 Nosek, B. A., Banaji, M. R., &#38; Greenwald,
                                 A. G. (2002). Harvesting implicit group
                                 attitudes and beliefs from a demonstration web
                                 site.<em>Group Dynamics&#58; Theory, Research,
                                 and Practice, 6</em>(1), 101-115.
                                 doi: 10.1037/1089-2699.6.1.101
                                 </p>
                                 <h3> Contacts </h3>
                                 <p>
                                 This App was developed by Ottavia M. Epifania
                                 at the University of Padova (Italy).
                                 For any further information on the App functioning or
				 for any problems regarding the App,
                                 please contact me at: otta.epifania@gmail.com
                                 or marinaottavia.epifania@phd.unipd.it
                                 </p>
                                 <h3> License </h3>
                                 <p>
                                 This app is a free software, and you can redistribute it and or modify it under the terms of the  <a href=https://opensource.org/licenses/MIT>MIT license</a>.
                                 <p>
                                 </blockquote>"
            )
          ))
        ),
        # Results Panel
        tabPanel(
          h4("D- Score results"),
          # Graphic representation of the results
          plotOutput(
            "distribution",
            click = clickOpts(id = "plot1_click"), # define graph clicker
            brush = brushOpts(id = "plot1_brush") # define area selection
            
          ),
          # Download plot button
          fluidRow(column(3, 
                          offset = 9, 
                          downloadButton("down_plot", "Download Plot"))), 
          # Conditional panel for displaying the clicker or area results
          fluidRow(
            conditionalPanel(
              # Point clicker apperas only when the point graph is selected
              condition = "input.graph == '1'",
              column(4,
                     h5("Points"),
                     verbatimTextOutput("click_info")),
              # Graph area selection always appears
              column(4,
                     h5("Area"),
                     verbatimTextOutput("brush_info"))
              
            ),
            conditionalPanel(condition = "input.graph != '1'",
                             h5("Area"),
                             verbatimTextOutput("info_hist"))
            
          ),
          # Summary statistics of the results
          fluidRow(
            column(6,
                   h5("Summary"),
                   verbatimTextOutput("summary")),
            # Number of trials slower than 10,000 ms
            column(2,
                   h5("Trials > 10,000ms"),
                   verbatimTextOutput("slow")),
            # Number of trials faster than 400 ms
            column(2,
                   h5("Trials < 400ms"),
                   verbatimTextOutput("fast")),
            # Conditional panel for the number of participants exceeding the accuracy deletion percentage
            column(
              2,
              conditionalPanel(
                condition =  "input.accuracy_del == '2'",
                h5("Accuracy deletion"),
                verbatimTextOutput("mistakes")
              )
              
            ),
            # Conditional panel for the number of participants exceeding the 10 % of fast responses threshold
            conditionalPanel(condition = "input.sbjFast_del == '2'",
                             column(
                               2,
                               h5("Participants < 300ms"),
                               verbatimTextOutput("sbjFast")
                             ))
          ),
          # Practice-Test IAT reliability
          h5(
            "Practice - Test reliability",
            verbatimTextOutput("pt_reliability")
          )
        ),
        # Descriptive statistics Panel
        tabPanel(
          h4("Descriptive Statistics"),
          fluidRow(column(
            10,
            h5("Average response times"),
            verbatimTextOutput("mean.block")
          )),
          fluidRow(column(
            10, h5("Accuracy"),
            verbatimTextOutput("accuracy_block")
          ))
          
        )
        
      ))
    )
  )
)