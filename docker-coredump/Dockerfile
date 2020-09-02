FROM ubuntu:18.04
RUN apt-get update
RUN apt-get install -y build-essential

USER root
WORKDIR /home
RUN mkdir /cores
COPY hello.c ./hello.c
RUN gcc hello.c
CMD ./a.out
