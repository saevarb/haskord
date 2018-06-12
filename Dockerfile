FROM haskell:8.2.2
RUN stack setup --resolver=lts-11.13
RUN git clone https://github.com/saevarb/haskord
WORKDIR haskord/haskord-bot
RUN git
RUN stack build
CMD bash
# CMD cd haskord-bot && stack exec haskord
