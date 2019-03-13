# SATIATED study
Soiled Airway Tracheal Intubation and the Effectiveness of Decontamination by Paramedics (SATIATED) study

## What is the SATIATED study?
SATIATED is a randomised trial which aims to determine whether paramedic first-pass intubation success of a simulated contaminated airway improves following training in Suction Assisted Laryngoscopy and Airway Decontamination (SALAD).

## Can I replicate the data analysis?
Yes! I have provided the [raw data](https://github.com/RichardPilbery/r-satiated/raw/master/data/SATIATED-Results%20-%20Sheet1.csv) and created a docker image which will enable you to recreate the environment I used to undertake the analysis. Just follow these steps:

1. Install [Docker](https://www.docker.com/products/docker-desktop)
2. Run Docker
3. Copy the [Dockerfile](https://raw.githubusercontent.com/RichardPilbery/r-satiated/master/Dockerfile) and place it into an empty folder
3. Open a terminal window (or Powershell in Windoze) and change the directory to match the folder that contains the Dockerfile
4. Enter the following command: `docker build ./`
5. This might take a while, but you should end with a comment like *Successfully built IMAGEID*
6. Run the command: `docker run --rm -p  8787:8787 -e PASSWORD=password IMAGEID` where IMAGEID is the IMAGEID value from step 5
7. Open a browser window and head to: *http://127.0.0.1:8787*. Log in with the username: rstudio and password: password
8. In the browser window, you should see an RStudio environment. ![RStudio](https://github.com/RichardPilbery/r-satiated/raw/master/images/8-RStudio.png)
9. Scroll down to find the project file, called *r-satiated.Rproj* and click it. ![Open project](https://github.com/RichardPilbery/r-satiated/raw/master/images/9-Open-the-project.png)
10. Now find the *Session* menu option along the top of the page and select *Restart R*. ![Restart R](https://github.com/RichardPilbery/r-satiated/raw/master/images/10-Restart-R.png)
11. In the top-right hand window, click on the Build tab and then by clicking on the triangle next to the Build Book button, choose the *bookdown::pdf_book* option. ![Build book](https://github.com/RichardPilbery/r-satiated/raw/master/images/11-Build-PDF.png)
12. Be patient, this might take a minute, but once completed a new window with the rendered PDF should appear. ![View PDF](https://github.com/RichardPilbery/r-satiated/raw/master/images/12-View-PDF.png)

## Funding
This study has been funded by a College of Paramedics small research grant. If you are a full member of the College, you are eligible to apply. Visit the [Research and Development Advisory Committee](https://www.collegeofparamedics.co.uk/college-governance/structure/research_and_audit_committee) page.

## Trial registration
ClinicalTrials.gov identifier: [NCT03599687](https://clinicaltrials.gov/ct2/show/NCT03599687)

## Acknowledgements
This study could not have been undertaken without the support and participation of paramedics working for Yorkshire Ambulance Service NHS Trust. In addition, the study has been supported by the National Institute for Health Research Clinical Research Network: Yorkshire and Humber. Finally, thanks goes to Dr. Jim DuCanto for his advice and support.
