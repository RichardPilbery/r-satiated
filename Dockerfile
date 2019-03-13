# This Dockerfile will take a custom image based on rocker/verse
# Depending on the output, you need to change line 5 on 03-results.Rmd. For gitbook it should be "html", for Word, "pandoc" and for PDF, "latex"
# Note that extra headers and rows do not work in Word, so some of the tables won't render properly. Use PDF or gitbook to see them how I intended!

FROM richardpilbery/satiated
RUN cd /home/rstudio && wget https://github.com/RichardPilbery/r-satiated/archive/master.tar.gz && tar xvfz master.tar.gz && rm master.tar.gz && chown -R rstudio:rstudio /home/rstudio/r-satiated-master && chmod 755 /home/rstudio/r-satiated-master


# Note to self: Had to reinstall tinytex to get PDF file generation to work: https://yihui.name/tinytex/

# To install the image
# 1. Create a new directory and place this Dockerfile inside
# 2. Enter the command docker build ./
# 3. Take a note of the IMAGEID...the command line will have a comment like: 'Successfully built IMAGEID'
# 4. Run this command: docker run --rm -p  8787:8787 -e PASSWORD=password IMAGEID
# 5. Point your browser to http://127.0.0.1:8787 and log in with the username: rstudio and password:password

