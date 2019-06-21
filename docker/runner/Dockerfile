FROM amazonlinux:latest
MAINTAINER Jasper Van der Jeugt <m@jaspervdj.be>

RUN yum install -y gmp-devel

ENV HOME="/root"
WORKDIR /root

COPY bin/* /root/

ENTRYPOINT /root/icfp2019-v0
