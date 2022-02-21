FROM archlinux/base
RUN pacman -Syu --noconfirm --needed base-devel git pacman-contrib
RUN useradd -m -g users -G wheel -s /bin/bash agasson
RUN echo '%wheel ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER agasson
WORKDIR /home/agasson
CMD ["/usr/bin/bash"]
