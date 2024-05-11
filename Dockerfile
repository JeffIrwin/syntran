FROM alpine:3.18
#FROM alpine:3.19.1

# Alpine 3.18 has gfortran 12.  3.19 has gfortran 13 which doesn't work with
# syntran :(

RUN apk add gfortran musl-dev
RUN apk add libc6-compat  # syntran needs glibc compatibility, not alpine's musl libc
RUN apk add rlwrap        # nice to have

COPY . .

#************
## Build the fortran build system fpm from source.  Binaries are available but
## source might be more portable although slower.  Takes 3+ minutes
#RUN apk add git
#RUN git clone https://github.com/jeffirwin/fpm /fpm
#RUN cd /fpm && ./install.sh --prefix="/usr/"
#
## Build syntran
#RUN fpm install --prefix="/usr/"
#************

# Takes 30 seconds
RUN apk add cmake make
#RUN ./build.sh  # no bash on alpine :(

RUN cmake -S . -B "build" -DCMAKE_BUILD_TYPE=Debug
RUN cmake --build "build" --config Debug
#RUN cmake -S . -B "build" -DCMAKE_BUILD_TYPE=Release
#RUN cmake --build "build" --config Release

RUN cp ./build/syntran /usr/bin/

#************

ENTRYPOINT ["rlwrap", "syntran"]

