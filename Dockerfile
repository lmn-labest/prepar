# Compilando o metis

FROM gcc:13.1.0
WORKDIR /usr/metislib/

COPY metisLib/metis-5.1.0.tar.gz ./
RUN tar -xvzf metis-5.1.0.tar.gz

RUN set -ex;                  \
    apt-get update;           \
    apt-get install -y cmake

RUN cd metis-5.1.0;           \
    make config;              \
    make


# Compilando o prepar

FROM gcc:13.1.0
WORKDIR /usr/prepar/

COPY src src
COPY include include
COPY --from=0 /usr/metislib/metis-5.1.0/build/Linux-x86_64/libmetis/libmetis.a lib/
COPY contrib/Makefile_base Makefile

RUN make


# Gerando a imagem final com apenas o prepar
FROM alpine:3.18.0
WORKDIR /usr/build/
COPY --from=1 /usr/prepar/bin/prepar ./

ENTRYPOINT [ "./prepar" ]
