
#FROM alpine:3.18
#FROM alpine:3.19.1
FROM alpine:3.21

WORKDIR /workdir

RUN apk add gfortran musl-dev
RUN apk add libc6-compat  # syntran needs glibc compatibility, not alpine's musl libc
RUN apk add rlwrap        # nice to have
RUN apk add curl

COPY src src
COPY CMakeLists.txt .
COPY fpm.toml .

#************
## Build the fortran build system fpm from source.  Takes 3+ minutes
#RUN apk add git
#RUN git clone https://github.com/jeffirwin/fpm /fpm
#RUN cd /fpm && ./install.sh --prefix="/usr/"
#
## Build syntran
#RUN fpm install --prefix="/usr/"
#************

# Install fpm from binary
ARG FPM_URL="https://github.com/fortran-lang/fpm/releases/download/v0.12.0/fpm-0.12.0-linux-x86_64-gcc-12"
RUN curl -L $FPM_URL -o fpm
RUN chmod +x fpm
RUN mv fpm /usr/local/bin
RUN fpm --version

RUN fpm install --prefix="/usr/"
RUN fpm test test

##************
## Takes 30 seconds
#RUN apk add cmake make
##RUN ./build.sh  # no bash on alpine :(
#
#RUN cmake -S . -B "build" -DCMAKE_BUILD_TYPE=Debug
#RUN cmake --build "build" --config Debug
##RUN cmake -S . -B "build" -DCMAKE_BUILD_TYPE=Release
##RUN cmake --build "build" --config Release
#
#RUN ./build/Release/test
#
#RUN cp ./build/Release/syntran /usr/bin/
##************

ENTRYPOINT ["rlwrap", "syntran"]

