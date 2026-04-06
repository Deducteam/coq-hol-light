FROM ocaml/opam:debian-12-ocaml-5.3
LABEL authors=Deducteam

RUN sudo apt update
RUN sudo apt install -y perl pkg-config libpcre2-dev m4 libgmp-dev libev-dev libssl-dev
RUN git clone --depth 1 https://github.com/Alidra/coq-hol-light.git
# COPY --chown=opam:opam . /home/opam/coq-hol-light
RUN export HOLLIGHT_DIR=`pwd`/hol-light
RUN export HOL2DK_DIR=`pwd`/hol2dk

WORKDIR $HOME/coq-hol-light
RUN ./reproduce --only 2
RUN ./reproduce --only 3
RUN ./reproduce --only 4
RUN ./reproduce --only 5
RUN ./reproduce --only 6
RUN ./reproduce --only 7
RUN ./reproduce --only 8
RUN ./reproduce --only 9

ENTRYPOINT ["./reproduce"]
CMD ["-y", "10"]