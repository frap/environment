FROM ubuntu:22.04
RUN apt-get update && \
    apt-get install --no-install-recommends -y make nasm && \
    rm -rf /var/lib/apt/lists/*

RUN useradd -m -g users -G wheel -s /bin/bash agasson
RUN echo '%wheel ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER agasson
WORKDIR /opt/eru
COPY ./eru-new.sh /opt/eru/eru-new.sh
CMD ["/usr/bin/bash"]
