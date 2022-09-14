FROM ubuntu:22.04
RUN apt-get update && \
    apt-get install --no-install-recommends -y make nasm && \
    rm -rf /var/lib/apt/lists/*

RUN adduser --system --shell /bin/bash --home /opt/eru agasson 
USER agasson
WORKDIR /opt/eru
COPY ./eru-new.sh /opt/eru/eru-new.sh
CMD ["/usr/bin/bash"]
