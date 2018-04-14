FROM mitchty/alpine-ghc:7.10 as build

RUN apk update && apk --no-cache add \
    make \
    musl-dev \
    util-linux-dev \
    zlib-dev


COPY ./cabal.sandbox.config Horus.cabal /build/
WORKDIR /build
RUN cabal sandbox init && cabal update && cabal install --only-dependencies

COPY Makefile /build/
COPY src /build/src
RUN make

FROM nginx:alpine

RUN apk update && apk --no-cache add gmp util-linux

COPY --from=build /build/bin/server /opt/horus/horus

COPY conf/nginx.conf /etc/nginx/

COPY src/resources /var/www/
COPY src/app /var/www/app

VOLUME ["/opt/horus/state"]

CMD ["sh", "-c", "/opt/horus/horus & nginx -g 'daemon off;'"]
