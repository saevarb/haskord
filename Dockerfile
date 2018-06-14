FROM haskell:8.2.2
RUN stack update --resolver=lts-11.13
RUN apt-get update && apt-get install unzip
COPY repo.zip /repo.zip
RUN unzip repo.zip
WORKDIR haskord-bot
RUN stack build
CMD stack exec haskord
