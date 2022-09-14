FROM archlinux/base
MAINTAINER Andres Gasson <gas@tuatara.red>
RUN pacman -Syu --noconfirm --needed base-devel git pacman-contrib
RUN useradd -m -g users -G wheel -s /bin/bash agasson
RUN echo '%wheel ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER agasson
WORKDIR /opt/eru
COPY ./eru-new.sh /opt/eru/eru-new.sh
CMD ["/usr/bin/bash"]
