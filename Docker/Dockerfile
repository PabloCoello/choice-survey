FROM ubuntu:18.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
                git \
                cmake \
                software-properties-common \
                python-minimal \
                python3-pip \
                libpq-dev \
                libspatialindex-dev

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
RUN apt-get update && apt-get install -y r-base

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv 68818C72E52529D4
RUN echo "deb http://repo.mongodb.org/apt/ubuntu bionic/mongodb-org/4.0 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-4.0.list
RUN apt-get update && apt-get install -y mongodb-org

RUN R -e "install.packages('plyr',dependencies=TRUE, repos='http://cran.rstudio.com/')"\
    && R -e "install.packages('parallel',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('doSNOW',dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('syuzhet',dependencies=TRUE, repos='http://cran.rstudio.com/')"

RUN python3 -m pip install pymongo \
                        sqlalchemy \
                        pandas \
                        geopandas \
                        nltk \
                        tweet-preprocessor \
                        rpy2 \
                        afinn \
                        vaderSentiment \
                        labMTsimple \
                        Rtree \
                        psycopg2

RUN python3 -m pip install "dask[distributed]" --upgrade
RUN python3 -m pip install "dask[dataframe]" --upgrade

RUN mkdir -p /local/lib/python2.7/dist-packages/

ENV PYTHONPATH=/local/lib/python2.7/dist-packages/

WORKDIR /opt/global-sentinel/