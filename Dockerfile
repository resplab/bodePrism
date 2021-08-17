FROM opencpu/base
RUN R -e 'remotes::install_github("resplab/bode")'
RUN R -e 'remotes::install_github("resplab/bodePrism")'
RUN echo "opencpu:opencpu" | chpasswd
