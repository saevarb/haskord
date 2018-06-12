FROM alpine:latest
ARG HASKORD_PATH
ARG MUEVAL_PATH
ARG CONFIG_PATH
RUN apk update
RUN apk add gmp ncurses libc6-compat
COPY $HASKORD_PATH /
COPY $MUEVAL_PATH /
COPY $CONFIG_PATH /
CMD ./haskord
