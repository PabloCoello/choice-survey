FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    git \
    cmake \
    software-properties-common \
    libpq-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    default-jre \
    default-jdk \
    locales

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
RUN apt-get update && apt-get install -y r-base

RUN R CMD javareconf

RUN R -e "install.packages('idefix',dependencies=TRUE, repos='http://cran.rstudio.com/')"\
    && R -e "install.packages('parallel',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('doSNOW',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('rjson',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('utf8',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('xml2',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('Rchoice',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('stringr',dependencies=TRUE, repos='http://cran.rstudio.com/')" 

WORKDIR /opt/choice-survey/