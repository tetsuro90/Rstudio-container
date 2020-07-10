FROM rocker/rstudio:latest

RUN apt-get update \
    && apt-get -y install \
        devscripts \
        zlib1g-dev


RUN install2.r --error \
    devtools


COPY .Rprofile /home/rstudio
COPY rstudio-prefs.json /home/rstudio/.config/rstudio/rstudio-prefs.json 

CMD ["/init"]
