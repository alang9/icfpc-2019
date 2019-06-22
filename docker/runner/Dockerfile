FROM amazonlinux:latest
MAINTAINER Jasper Van der Jeugt <m@jaspervdj.be>

RUN yum install -y gmp-devel
RUN yum install -y awscli

ENV HOME="/root"
WORKDIR /root

RUN mkdir /root/bin
COPY bin/* /root/bin/

COPY entrypoint.sh /root/entrypoint.sh
RUN chmod +x /root/entrypoint.sh

ENTRYPOINT /root/entrypoint.sh
