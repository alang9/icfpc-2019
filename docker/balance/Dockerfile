FROM amazonlinux:latest
MAINTAINER Jasper Van der Jeugt <m@jaspervdj.be>

RUN yum install -y unzip python3-pip

ENV HOME="/root"
WORKDIR /root

RUN curl -O "https://icfpcontest2019.github.io/download/lambda-client.zip"
RUN unzip -j lambda-client.zip
RUN pip3 install -r requirements.txt

COPY entrypoint.sh /root/entrypoint.sh
RUN chmod +x /root/entrypoint.sh

ENTRYPOINT /root/entrypoint.sh
