
## Bike share demand

In this lab, you are going to analyze the bike rentals in Seoul, South Korea. You can suppose that the reader is an interested decision-maker within a bike rental company. This reader is generally familiar with statistical reasoning -- perhaps at the level of 203 -- they are willing to read justifications for the analysis that you produce, but they have **not** taken 271. 
- This lab has a strict 10-page limit. Please do not modify fontsize, margins, or line spacing to try to squeeze more into this report. Use the constraints to focus your analysis, and to require a clarity in what you report. 
- Because you have this constraint, your answers should be concise. The tension that you will have to manage is this requirement for concision against the requirement for complete argument. 

## Method of Submitting 

You must submit a PDF of your compiled analysis and your code (.rmd) to Gradescope.

## Guidance

- In this report, you _should not_ need to show code that produces figures or tables; but, you _should_ show code that estimates models and standard errors. 
  - But, you _should not_ show code that produces `ggplot()` or `stargazer()` tables.
- You should use appropriate report writing style. 
  - Language should be professional. This does not mean "fancy" or "academic" but rather that it should show respect for the reader. The Dalal, Fowlkes and Hoadley (1989) paper that you read is a good model of this style of writing. 
- You should use an appropriate reporting style. 
  - Tables, figures, and models that you present are the evidence for the argument that you are making. 
  - Every part of your argument needs to be supported by evidence. 
  - You should not dilute the effectiveness of your evidence by including superfluous evidence. A good test for this is to evaluate whether every table and figure is discussed in the text, and also whether every argument you make in the text has a corresponding piece of evidence that you can point your reader to. 
  - Because you have only 20 pages, you will have to carefully consider what plots to show, and how to show them. 
- If you do any data wrangling, manipulation, filtering, or other similar task, this should **not** happen inside the report. This work should be conducted in a separate, modular file so that your report begins with the clean dataset that you would like to work with. 
- If you do any data wrangling, manipulation, filtering, or other similar task, this **should** be reproducible, so that there is a record for how you got from the raw data to the final data that you analyze. 
- Please, try to constrain yourselves to core libraries that we have presented at this point in the course. Suppose that those have already been cleared by the house statisticans at NASA. If you choose to use other libraries, or functions from other libraries, you should (a) justify their necessity; (b) build the package installs in a reproducible way, so that someone on an arbitrary computer can reproduce your work. This could involve a Docker container for your work; or, it could involve using `renv`. 

Students are expected to act with regard to UC Berkeley Academic Integrity.
