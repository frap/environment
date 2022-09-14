FROM ubuntu:22.04
RUN apt-get update && \
    apt-get install --no-install-recommends -y git curl openssh-client && \
    rm -rf /var/lib/apt/lists/*

RUN adduser --system --shell /usr/bin/bash --home /opt/eru agasson 
USER agasson
WORKDIR /opt/eru
COPY ./eru-new.sh /opt/eru/eru-new.sh
COPY ./install-bb /opt/eru/install-bb
RUN ssh-keygen -t rsa -b 2048 -N "" -f ~/.ssh/id_rsa
#RUN  bash -c "/opt/eru/install-bb"
RUN   bash -c "bash < <(curl -s https://raw.githubusercontent.com/babashka/babashka/master/install)"
CMD ["/usr/bin/bash"]
