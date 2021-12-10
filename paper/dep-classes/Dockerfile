# Lets try this
FROM archlinux

RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm sbt z3 git

RUN git clone https://github.com/stg-tud/dep-classes-smt.git /opt/dep-classes-smt

CMD ["echo","yay"]
