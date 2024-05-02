FROM alpine:3.19.1

RUN apk add gfortran musl-dev
RUN apk add git
RUN apk add libc6-compat  # syntran needs glibc compatibility, not alpine's musl libc
RUN apk add rlwrap        # nice to have

COPY . .

# Build the fortran build system fpm from source.  Binaries are available but
# source might be more portable although slower
RUN git clone https://github.com/jeffirwin/fpm /fpm
RUN cd /fpm && ./install.sh --prefix="/usr/"

# Build syntran
RUN fpm install --prefix="/usr/"

ENTRYPOINT ["rlwrap", "syntran"]

