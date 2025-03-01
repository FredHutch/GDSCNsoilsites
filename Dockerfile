FROM fredhutch/r-shiny-server-base:4.4.1
RUN apt-get update -y

# Devtools dependencies
RUN apt-get install -y libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev curl libxml2 libxml2-dev libcurl4-openssl-dev libssl-dev r-cran-openssl  libgit2-dev
RUN Rscript -e "install.packages('systemfonts')"
RUN Rscript -e "install.packages('textshaping')"
RUN Rscript -e "install.packages('ragg')"

# gdal dependencies
RUN apt-get install -y gdal-bin proj-bin libgdal-dev libproj-dev

# Load devtools and custom package
RUN Rscript -e "install.packages('devtools', dependencies=T)"
RUN Rscript -e "install.packages('leaflet', dependencies=T)"
RUN Rscript -e "install.packages('stringr', dependencies=T)"
RUN Rscript -e "install.packages('dplyr', dependencies=T)"
RUN Rscript -e "install.packages('tidyr', dependencies=T)"
RUN Rscript -e "install.packages('lubridate', dependencies=T)"
RUN Rscript -e "install.packages('evaluate', dependencies=T)"
RUN Rscript -e "install.packages('googlesheets4', dependencies=T)"
RUN Rscript -e "install.packages('shinydashboard', dependencies=T)"
RUN Rscript -e "install.packages('ggplot2', dependencies=T)"
# RUN Rscript -e "library(devtools);install_github('FredHutch/GDSCNsoilsites')"

# install the package
ADD . /src/GDSCNsoilsites
WORKDIR /src/GDSCNsoilsites
RUN R CMD INSTALL .
WORKDIR /srv/shiny-server/
RUN rm -rf /src/GDSCNsoilsites

RUN rm -rf /srv/shiny-server/
RUN mkdir -p /src/shiny-server/
COPY app/start.R /srv/shiny-server/app.R
COPY data /srv/shiny-server/data/
COPY www /srv/shiny-server/www/

RUN chown -R shiny:shiny /srv/shiny-server/
EXPOSE 3838
WORKDIR /srv/shiny-server/

ADD .secrets /srv/shiny-server/.secrets
RUN chown -R shiny:shiny .secrets

ENV APPLICATION_LOGS_TO_STDOUT=true
ENV SHINY_LOG_STDERR=1

#CMD /usr/bin/shiny-server
CMD R -f app.R

