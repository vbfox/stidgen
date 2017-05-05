FROM mono:4.8.0.495

WORKDIR /stidgen

COPY . /stidgen

RUN ./build.sh
