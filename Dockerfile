FROM haskell:8.2.2
RUN stack update --resolver=lts-11.13
RUN git clone https://github.com/saevarb/haskord
WORKDIR haskord/haskord-bot
RUN git checkout containerization
RUN stack build
COPY haskord-bot/config.yaml /haskord/haskord-bot/
CMD bash
# CMD cd haskord-bot && stack exec haskord
