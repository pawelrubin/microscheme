FROM haskell:8.8.4

WORKDIR /app

RUN apt-get update
RUN apt-get install -y wget lsb-release software-properties-common

RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN ./llvm.sh 9

COPY . .

RUN stack build
